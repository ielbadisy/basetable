// Native in-memory table engine for basetable verbs.
//
// R keeps the public, base-style API and expression evaluation. This layer owns
// the hot-path table mechanics: projection, row materialisation, ordering,
// distinct/duplicate detection, and grouped row counts.

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <algorithm>
#include <climits>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <string>
#include <thread>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace {

enum AggFun {
  AGG_SUM = 0,
  AGG_MEAN = 1,
  AGG_MIN = 2,
  AGG_MAX = 3,
  AGG_VAR = 4,
  AGG_SD = 5,
  AGG_N = 6
};

struct Frame {
  R_xlen_t nrow;
  R_xlen_t ncol;
};

Frame frame_from(SEXP x) {
  if (!Rf_isNewList(x) || !Rf_inherits(x, "data.frame"))
    Rf_error("basetable: expected a data.frame");
  R_xlen_t ncol = Rf_xlength(x);
  R_xlen_t nrow = 0;
  if (ncol > 0) {
    nrow = Rf_xlength(VECTOR_ELT(x, 0));
    for (R_xlen_t j = 1; j < ncol; ++j)
      if (Rf_xlength(VECTOR_ELT(x, j)) != nrow)
        Rf_error("basetable: columns must have equal length");
  } else {
    SEXP rn = Rf_getAttrib(x, R_RowNamesSymbol);
    if (TYPEOF(rn) == INTSXP && Rf_xlength(rn) == 2 && INTEGER(rn)[0] == NA_INTEGER)
      nrow = -INTEGER(rn)[1];
  }
  return { nrow, ncol };
}

std::vector<int> col_index(SEXP s_cols, R_xlen_t ncol) {
  std::vector<int> cols;
  if (Rf_isNull(s_cols)) {
    cols.reserve((size_t)ncol);
    for (R_xlen_t j = 0; j < ncol; ++j) cols.push_back((int)j);
    return cols;
  }
  if (TYPEOF(s_cols) != INTSXP && TYPEOF(s_cols) != REALSXP)
    Rf_error("basetable: column index must be integer");
  R_xlen_t n = Rf_xlength(s_cols);
  cols.reserve((size_t)n);
  for (R_xlen_t i = 0; i < n; ++i) {
    int v = TYPEOF(s_cols) == INTSXP ? INTEGER(s_cols)[i] : (int)REAL(s_cols)[i];
    if (v == NA_INTEGER || v < 1 || v > ncol)
      Rf_error("basetable: column index out of bounds");
    cols.push_back(v - 1);
  }
  return cols;
}

std::vector<R_xlen_t> row_index(SEXP s_rows, R_xlen_t nrow) {
  std::vector<R_xlen_t> rows;
  if (Rf_isNull(s_rows)) {
    rows.reserve((size_t)nrow);
    for (R_xlen_t i = 0; i < nrow; ++i) rows.push_back(i);
    return rows;
  }
  if (TYPEOF(s_rows) == LGLSXP) {
    if (Rf_xlength(s_rows) != nrow)
      Rf_error("basetable: logical row index has wrong length");
    rows.reserve((size_t)nrow);
    const int* p = LOGICAL(s_rows);
    for (R_xlen_t i = 0; i < nrow; ++i)
      if (p[i] == TRUE) rows.push_back(i);
    return rows;
  }
  if (TYPEOF(s_rows) == INTSXP || TYPEOF(s_rows) == REALSXP) {
    R_xlen_t n = Rf_xlength(s_rows);
    rows.reserve((size_t)n);
    for (R_xlen_t i = 0; i < n; ++i) {
      R_xlen_t v = TYPEOF(s_rows) == INTSXP ? INTEGER(s_rows)[i] : (R_xlen_t)REAL(s_rows)[i];
      if (v == NA_INTEGER || v < 1 || v > nrow)
        Rf_error("basetable: row index out of bounds");
      rows.push_back(v - 1);
    }
    return rows;
  }
  Rf_error("basetable: unsupported row index");
}

SEXP make_row_names(R_xlen_t n) {
  SEXP rn = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(rn)[0] = NA_INTEGER;
  INTEGER(rn)[1] = (n > INT_MAX) ? NA_INTEGER : -(int)n;
  UNPROTECT(1);
  return rn;
}

// Every frame the in-memory engine returns carries basetable's own class.
void set_table_class(SEXP out) {
  SEXP cls = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(cls, 0, Rf_mkChar("basetable"));
  SET_STRING_ELT(cls, 1, Rf_mkChar("data.frame"));
  Rf_setAttrib(out, R_ClassSymbol, cls);
  UNPROTECT(1);
}

void copy_common_attrs(SEXP out, SEXP in) {
  for (SEXP a = ATTRIB(in); a != R_NilValue; a = CDR(a)) {
    SEXP tag = TAG(a);
    if (tag == R_NamesSymbol || tag == R_DimSymbol || tag == R_DimNamesSymbol) continue;
    Rf_setAttrib(out, tag, CAR(a));
  }
}

SEXP subset_vector(SEXP col, const std::vector<R_xlen_t>& rows) {
  R_xlen_t n = (R_xlen_t)rows.size();
  SEXP out = PROTECT(Rf_allocVector(TYPEOF(col), n));
  switch (TYPEOF(col)) {
    case LGLSXP: {
      const int* src = LOGICAL(col);
      int* dst = LOGICAL(out);
      for (R_xlen_t i = 0; i < n; ++i) dst[i] = src[rows[(size_t)i]];
      break;
    }
    case INTSXP: {
      const int* src = INTEGER(col);
      int* dst = INTEGER(out);
      for (R_xlen_t i = 0; i < n; ++i) dst[i] = src[rows[(size_t)i]];
      break;
    }
    case REALSXP: {
      const double* src = REAL(col);
      double* dst = REAL(out);
      for (R_xlen_t i = 0; i < n; ++i) dst[i] = src[rows[(size_t)i]];
      break;
    }
    case STRSXP:
      for (R_xlen_t i = 0; i < n; ++i)
        SET_STRING_ELT(out, i, STRING_ELT(col, rows[(size_t)i]));
      break;
    case VECSXP:
      for (R_xlen_t i = 0; i < n; ++i)
        SET_VECTOR_ELT(out, i, VECTOR_ELT(col, rows[(size_t)i]));
      break;
    default:
      UNPROTECT(1);
      Rf_error("basetable: unsupported column type '%s'", Rf_type2char(TYPEOF(col)));
  }
  copy_common_attrs(out, col);
  UNPROTECT(1);
  return out;
}

SEXP build_frame(SEXP df, const std::vector<R_xlen_t>& rows, const std::vector<int>& cols) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t)cols.size()));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t)cols.size()));
  SEXP old_names = Rf_getAttrib(df, R_NamesSymbol);
  for (R_xlen_t j = 0; j < (R_xlen_t)cols.size(); ++j) {
    int src_j = cols[(size_t)j];
    SET_VECTOR_ELT(out, j, subset_vector(VECTOR_ELT(df, src_j), rows));
    SET_STRING_ELT(names, j, STRING_ELT(old_names, src_j));
  }
  Rf_setAttrib(out, R_NamesSymbol, names);
  Rf_setAttrib(out, R_RowNamesSymbol, make_row_names((R_xlen_t)rows.size()));
  set_table_class(out);
  UNPROTECT(2);
  return out;
}

// One numeric key slot: logical / integer / real all fold into the same double
// domain so that 1L, 1.0 and TRUE compare equal across columns, matching what
// base::merge() does when it coerces mismatched key types.
bool key_is_numeric(SEXP col) {
  int t = TYPEOF(col);
  return (t == LGLSXP || t == INTSXP || t == REALSXP) && !Rf_isFactor(col);
}

double key_num(SEXP col, R_xlen_t i, int& kind) {
  // kind: 0 finite, 1 NA, 2 NaN
  switch (TYPEOF(col)) {
    case LGLSXP: {
      int v = LOGICAL(col)[i];
      if (v == NA_LOGICAL) { kind = 1; return 0.0; }
      kind = 0; return (double)v;
    }
    case INTSXP: {
      int v = INTEGER(col)[i];
      if (v == NA_INTEGER) { kind = 1; return 0.0; }
      kind = 0; return (double)v;
    }
    default: {  // REALSXP
      double v = REAL(col)[i];
      if (ISNA(v)) { kind = 1; return 0.0; }
      if (std::isnan(v)) { kind = 2; return 0.0; }
      kind = 0; return v == 0.0 ? 0.0 : v;  // normalise -0.0
    }
  }
}

// A key slot rendered as a string: factor label, string element, or NA.
SEXP key_str_elt(SEXP col, R_xlen_t i) {
  if (Rf_isFactor(col)) {
    SEXP lv = Rf_getAttrib(col, R_LevelsSymbol);
    int code = INTEGER(col)[i];
    if (code == NA_INTEGER || code < 1 || code > Rf_length(lv)) return NA_STRING;
    return STRING_ELT(lv, code - 1);
  }
  if (TYPEOF(col) == STRSXP) return STRING_ELT(col, i);
  return NA_STRING;
}

// Like key_str_elt but also coerces numeric / logical slots to their character
// form, the way rbind() / as.character() would when binding mixed columns.
SEXP key_str_or_coerce(SEXP col, R_xlen_t i) {
  if (Rf_isFactor(col) || TYPEOF(col) == STRSXP) return key_str_elt(col, i);
  char buf[32];
  switch (TYPEOF(col)) {
    case LGLSXP: {
      int v = LOGICAL(col)[i];
      return v == NA_LOGICAL ? NA_STRING : Rf_mkChar(v ? "TRUE" : "FALSE");
    }
    case INTSXP: {
      int v = INTEGER(col)[i];
      if (v == NA_INTEGER) return NA_STRING;
      std::snprintf(buf, sizeof(buf), "%d", v);
      return Rf_mkChar(buf);
    }
    case REALSXP: {
      double v = REAL(col)[i];
      if (ISNAN(v)) return NA_STRING;
      std::snprintf(buf, sizeof(buf), "%.15g", v);
      return Rf_mkChar(buf);
    }
    default:
      return NA_STRING;
  }
}

bool parse_agg_fun(SEXP s_fun, AggFun& fun) {
  if (TYPEOF(s_fun) != STRSXP || Rf_xlength(s_fun) != 1) return false;
  const char* f = CHAR(STRING_ELT(s_fun, 0));
  if (!std::strcmp(f, "sum")) { fun = AGG_SUM; return true; }
  if (!std::strcmp(f, "mean")) { fun = AGG_MEAN; return true; }
  if (!std::strcmp(f, "min")) { fun = AGG_MIN; return true; }
  if (!std::strcmp(f, "max")) { fun = AGG_MAX; return true; }
  if (!std::strcmp(f, "var")) { fun = AGG_VAR; return true; }
  if (!std::strcmp(f, "sd")) { fun = AGG_SD; return true; }
  if (!std::strcmp(f, "n") || !std::strcmp(f, "length")) { fun = AGG_N; return true; }
  return false;
}

struct AggState {
  double sum = 0.0;
  double sq = 0.0;
  double min = R_PosInf;
  double max = R_NegInf;
  int n = 0;
  bool bad = false;
};

double value_as_double(SEXP col, R_xlen_t i, bool& na) {
  na = false;
  switch (TYPEOF(col)) {
    case LGLSXP: {
      int v = LOGICAL(col)[i];
      if (v == NA_LOGICAL) { na = true; return NA_REAL; }
      return (double)v;
    }
    case INTSXP: {
      int v = INTEGER(col)[i];
      if (v == NA_INTEGER) { na = true; return NA_REAL; }
      return (double)v;
    }
    case REALSXP: {
      double v = REAL(col)[i];
      if (ISNAN(v)) { na = true; return NA_REAL; }
      return v;
    }
    default:
      Rf_error("basetable: aggregate value columns must be numeric, integer, or logical");
  }
}

double agg_finish(const AggState& s, AggFun fun, bool na_rm) {
  if (!na_rm && s.bad) return NA_REAL;
  switch (fun) {
    case AGG_SUM:
      return s.sum;
    case AGG_MEAN:
      return s.n > 0 ? s.sum / s.n : NA_REAL;
    case AGG_MIN:
      return s.n > 0 ? s.min : NA_REAL;
    case AGG_MAX:
      return s.n > 0 ? s.max : NA_REAL;
    case AGG_VAR:
      return s.n > 1 ? (s.sq - s.sum * s.sum / s.n) / (s.n - 1) : NA_REAL;
    case AGG_SD:
      return s.n > 1 ? std::sqrt((s.sq - s.sum * s.sum / s.n) / (s.n - 1)) : NA_REAL;
    case AGG_N:
      return (double)s.n;
  }
  return NA_REAL;
}

void agg_update(AggState& s, AggFun fun, double x) {
  switch (fun) {
    case AGG_SUM:
      s.sum += x;
      break;
    case AGG_MEAN:
      s.sum += x;
      ++s.n;
      break;
    case AGG_MIN:
      if (x < s.min) s.min = x;
      ++s.n;
      break;
    case AGG_MAX:
      if (x > s.max) s.max = x;
      ++s.n;
      break;
    case AGG_VAR:
    case AGG_SD:
      s.sum += x;
      s.sq += x * x;
      ++s.n;
      break;
    case AGG_N:
      ++s.n;
      break;
  }
}

void agg_merge(AggState& a, const AggState& b) {
  a.sum += b.sum;
  a.sq += b.sq;
  a.n += b.n;
  if (b.min < a.min) a.min = b.min;
  if (b.max > a.max) a.max = b.max;
  if (b.bad) a.bad = true;
}

int clamp_threads(SEXP s_n_threads, R_xlen_t work, R_xlen_t min_work) {
  int req = 1;
  if (s_n_threads != R_NilValue && Rf_xlength(s_n_threads) >= 1) {
    int v = Rf_asInteger(s_n_threads);
    if (v != NA_INTEGER && v > 0) req = v;
  }
  if (req < 1) req = 1;
  if (req > 64) req = 64;
  if (work < min_work) return 1;
  int by_work = (int)std::min<R_xlen_t>(req, work / min_work);
  return by_work < 1 ? 1 : by_work;
}


// ---- composite key codec ----------------------------------------------------
//
// Encodes the key columns of a row into a small fixed-width integer tuple so
// grouping and joins use a plain hash map instead of hashing a fresh byte
// string per row. Numeric columns (logical/integer/double) collapse into one
// value domain; string and factor columns are dictionary encoded. A codec can
// be reused across two frames (join build vs probe): after freeze(), a string
// not seen on the build side gets a unique negative code that never matches.

using KeyBuf = std::vector<int64_t>;

struct KeyHash {
  size_t operator()(const KeyBuf& k) const noexcept {
    size_t h = 1469598103934665603ULL;
    for (int64_t v : k) {
      h ^= (size_t)(uint64_t)v;
      h *= 1099511628211ULL;
      h ^= h >> 29;
    }
    return h;
  }
};

inline int64_t real_slot(double d) {
  if (ISNA(d)) return (int64_t)0x7ff00000000007a2LL;
  if (std::isnan(d)) return (int64_t)0x7ff00000000007a3LL;
  if (d == 0.0) d = 0.0;  // fold -0.0
  int64_t bits;
  std::memcpy(&bits, &d, sizeof(bits));
  return bits;
}

struct KeyCodec {
  // per slot: false = numeric domain (lgl/int/real), true = string domain
  // (character or factor, matched by label so the two interoperate)
  std::vector<char> is_str;
  std::vector<std::unordered_map<const void*, int>> dict;
  bool frozen = false;
  int miss_code = -2;

  static bool str_kind(SEXP col) {
    return Rf_isFactor(col) || TYPEOF(col) == STRSXP;
  }
  static bool num_kind(SEXP col) {
    return !str_kind(col) &&
      (TYPEOF(col) == LGLSXP || TYPEOF(col) == INTSXP || TYPEOF(col) == REALSXP);
  }

  KeyCodec(SEXP df, const std::vector<int>& cols) {
    is_str.resize(cols.size());
    dict.resize(cols.size());
    for (size_t c = 0; c < cols.size(); ++c) {
      SEXP col = VECTOR_ELT(df, cols[c]);
      if (str_kind(col)) is_str[c] = 1;
      else if (num_kind(col)) is_str[c] = 0;
      else Rf_error("basetable: unsupported key column type '%s'", Rf_type2char(TYPEOF(col)));
    }
  }

  // Widen the domain of each slot so a second frame's columns encode the same
  // way (a string column on either side makes the slot a string slot).
  void unify(SEXP df, const std::vector<int>& cols) {
    for (size_t c = 0; c < cols.size() && c < is_str.size(); ++c) {
      if (str_kind(VECTOR_ELT(df, cols[c]))) is_str[c] = 1;
    }
  }

  void freeze() { frozen = true; }

  int64_t str_slot(size_t c, SEXP s) {
    if (s == NA_STRING) return -1;
    const void* p = (const void*)s;
    auto& d = dict[c];
    auto it = d.find(p);
    if (it != d.end()) return it->second;
    if (frozen) return (int64_t)(miss_code--);
    int code = (int)d.size();
    d.emplace(p, code);
    return code;
  }

  void encode(SEXP df, const std::vector<int>& cols, R_xlen_t i, KeyBuf& out) {
    out.resize(cols.size());
    for (size_t c = 0; c < cols.size(); ++c) {
      SEXP col = VECTOR_ELT(df, cols[c]);
      if (!is_str[c]) {
        if (!num_kind(col)) { out[c] = (int64_t)(miss_code--); continue; }
        int kd = 0;
        double v = key_num(col, i, kd);
        out[c] = kd == 0 ? real_slot(v) : (kd == 2 ? real_slot(R_NaN) : INT64_MIN);
        continue;
      }
      SEXP s;
      if (Rf_isFactor(col)) {
        SEXP lv = Rf_getAttrib(col, R_LevelsSymbol);
        int code = INTEGER(col)[i];
        s = (code == NA_INTEGER || code < 1 || code > Rf_length(lv))
          ? NA_STRING : STRING_ELT(lv, code - 1);
      } else if (TYPEOF(col) == STRSXP) {
        s = STRING_ELT(col, i);
      } else {
        out[c] = (int64_t)(miss_code--);
        continue;
      }
      out[c] = str_slot(c, s);
    }
  }
};

// Single-column grouping: per-row 0-based group code plus the first row index
// per group, in first-seen order. Strings hash by interned CHARSXP address and
// factors by level code, so neither builds a byte key. Returns false for
// column types the dense integer path in *_single already covers.
bool group_single(SEXP col, R_xlen_t nrow, std::vector<int>& codes, std::vector<R_xlen_t>& first) {
  codes.resize((size_t)nrow);
  if (Rf_isFactor(col)) {
    std::unordered_map<int, int> d;
    d.reserve((size_t)nrow);
    for (R_xlen_t i = 0; i < nrow; ++i) {
      int lc = INTEGER(col)[i];
      auto it = d.find(lc);
      int c;
      if (it == d.end()) { c = (int)first.size(); d.emplace(lc, c); first.push_back(i); }
      else c = it->second;
      codes[(size_t)i] = c;
    }
    return true;
  }
  if (TYPEOF(col) == STRSXP) {
    std::unordered_map<const void*, int> d;
    d.reserve((size_t)nrow);
    for (R_xlen_t i = 0; i < nrow; ++i) {
      const void* s = (const void*)STRING_ELT(col, i);
      auto it = d.find(s);
      int c;
      if (it == d.end()) { c = (int)first.size(); d.emplace(s, c); first.push_back(i); }
      else c = it->second;
      codes[(size_t)i] = c;
    }
    return true;
  }
  if (TYPEOF(col) == REALSXP) {
    std::unordered_map<int64_t, int> d;
    d.reserve((size_t)nrow);
    const double* p = REAL(col);
    for (R_xlen_t i = 0; i < nrow; ++i) {
      int64_t s = real_slot(p[i]);
      auto it = d.find(s);
      int c;
      if (it == d.end()) { c = (int)first.size(); d.emplace(s, c); first.push_back(i); }
      else c = it->second;
      codes[(size_t)i] = c;
    }
    return true;
  }
  return false;
}

bool unique_int_dense(const int* p, R_xlen_t nrow, std::vector<R_xlen_t>& rows) {
  if (nrow == 0) return true;
  int minv = INT_MAX, maxv = INT_MIN;
  bool has_na = false;
  for (R_xlen_t i = 0; i < nrow; ++i) {
    int v = p[i];
    if (v == NA_INTEGER) {
      has_na = true;
    } else {
      if (v < minv) minv = v;
      if (v > maxv) maxv = v;
    }
  }
  if (minv == INT_MAX) {
    rows.push_back(0);
    return true;
  }
  uint64_t span = (uint64_t)((int64_t)maxv - (int64_t)minv + 1);
  if (span > (uint64_t)nrow * 4 || span > 10000000ULL) return false;

  std::vector<unsigned char> seen((size_t)span, 0);
  bool seen_na = false;
  rows.reserve((size_t)std::min<R_xlen_t>(nrow, (R_xlen_t)span + (has_na ? 1 : 0)));
  for (R_xlen_t i = 0; i < nrow; ++i) {
    int v = p[i];
    if (v == NA_INTEGER) {
      if (!seen_na) {
        seen_na = true;
        rows.push_back(i);
      }
    } else {
      size_t k = (size_t)((int64_t)v - (int64_t)minv);
      if (!seen[k]) {
        seen[k] = 1;
        rows.push_back(i);
      }
    }
  }
  return true;
}

template <typename T>
bool unique_hash_typed(const T* p, R_xlen_t nrow, std::vector<R_xlen_t>& rows) {
  std::unordered_set<T> seen;
  seen.reserve((size_t)nrow);
  rows.reserve((size_t)nrow);
  for (R_xlen_t i = 0; i < nrow; ++i) {
    if (seen.emplace(p[i]).second) rows.push_back(i);
  }
  return true;
}

bool unique_single(SEXP col, R_xlen_t nrow, std::vector<R_xlen_t>& rows) {
  switch (TYPEOF(col)) {
    case LGLSXP:
      return unique_int_dense(LOGICAL(col), nrow, rows) ||
             unique_hash_typed<int>(LOGICAL(col), nrow, rows);
    case INTSXP:
      return unique_int_dense(INTEGER(col), nrow, rows) ||
             unique_hash_typed<int>(INTEGER(col), nrow, rows);
    default:
      return false;
  }
}

bool duplicated_int_dense(const int* p, R_xlen_t nrow, bool from_last, SEXP out) {
  if (nrow == 0) return true;
  int minv = INT_MAX, maxv = INT_MIN;
  for (R_xlen_t i = 0; i < nrow; ++i) {
    int v = p[i];
    if (v == NA_INTEGER) continue;
    if (v < minv) minv = v;
    if (v > maxv) maxv = v;
  }
  if (minv == INT_MAX) {
    int* dst = LOGICAL(out);
    for (R_xlen_t i = 0; i < nrow; ++i) dst[i] = from_last ? i != nrow - 1 : i != 0;
    return true;
  }
  uint64_t span = (uint64_t)((int64_t)maxv - (int64_t)minv + 1);
  if (span > (uint64_t)nrow * 4 || span > 10000000ULL) return false;

  int* dst = LOGICAL(out);
  std::fill(dst, dst + nrow, FALSE);
  std::vector<unsigned char> seen((size_t)span, 0);
  bool seen_na = false;
  if (from_last) {
    for (R_xlen_t i = nrow; i-- > 0;) {
      int v = p[i];
      if (v == NA_INTEGER) {
        dst[i] = seen_na ? TRUE : FALSE;
        seen_na = true;
      } else {
        size_t k = (size_t)((int64_t)v - (int64_t)minv);
        dst[i] = seen[k] ? TRUE : FALSE;
        seen[k] = 1;
      }
    }
  } else {
    for (R_xlen_t i = 0; i < nrow; ++i) {
      int v = p[i];
      if (v == NA_INTEGER) {
        dst[i] = seen_na ? TRUE : FALSE;
        seen_na = true;
      } else {
        size_t k = (size_t)((int64_t)v - (int64_t)minv);
        dst[i] = seen[k] ? TRUE : FALSE;
        seen[k] = 1;
      }
    }
  }
  return true;
}

template <typename T>
bool duplicated_hash_typed(const T* p, R_xlen_t nrow, bool from_last, SEXP out) {
  int* dst = LOGICAL(out);
  std::fill(dst, dst + nrow, FALSE);
  std::unordered_set<T> seen;
  seen.reserve((size_t)nrow);
  if (from_last) {
    for (R_xlen_t i = nrow; i-- > 0;) {
      dst[i] = seen.emplace(p[i]).second ? FALSE : TRUE;
    }
  } else {
    for (R_xlen_t i = 0; i < nrow; ++i) {
      dst[i] = seen.emplace(p[i]).second ? FALSE : TRUE;
    }
  }
  return true;
}

bool duplicated_single(SEXP col, R_xlen_t nrow, bool from_last, SEXP out) {
  switch (TYPEOF(col)) {
    case LGLSXP:
      return duplicated_int_dense(LOGICAL(col), nrow, from_last, out) ||
             duplicated_hash_typed<int>(LOGICAL(col), nrow, from_last, out);
    case INTSXP:
      return duplicated_int_dense(INTEGER(col), nrow, from_last, out) ||
             duplicated_hash_typed<int>(INTEGER(col), nrow, from_last, out);
    default:
      return false;
  }
}

bool count_int_dense(SEXP df, int by, const int* p, R_xlen_t nrow, SEXP s_name, SEXP* out_ptr) {
  if (nrow == 0) return false;
  int minv = INT_MAX, maxv = INT_MIN;
  for (R_xlen_t i = 0; i < nrow; ++i) {
    int v = p[i];
    if (v == NA_INTEGER) continue;
    if (v < minv) minv = v;
    if (v > maxv) maxv = v;
  }
  if (minv == INT_MAX) return false;
  uint64_t span = (uint64_t)((int64_t)maxv - (int64_t)minv + 1);
  if (span > (uint64_t)nrow * 4 || span > 10000000ULL) return false;

  std::vector<int> counts((size_t)span, 0);
  std::vector<R_xlen_t> first_pos((size_t)span, -1);
  bool has_na = false;
  R_xlen_t na_first = -1;
  int na_count = 0;
  std::vector<R_xlen_t> first;
  first.reserve((size_t)std::min<R_xlen_t>(nrow, (R_xlen_t)span + 1));
  for (R_xlen_t i = 0; i < nrow; ++i) {
    int v = p[i];
    if (v == NA_INTEGER) {
      if (!has_na) {
        has_na = true;
        na_first = i;
        first.push_back(i);
      }
      ++na_count;
    } else {
      size_t k = (size_t)((int64_t)v - (int64_t)minv);
      if (counts[k] == 0) {
        first_pos[k] = i;
        first.push_back(i);
      }
      ++counts[k];
    }
  }

  std::vector<int> out_counts;
  out_counts.reserve(first.size());
  for (R_xlen_t idx : first) {
    int v = p[idx];
    if (v == NA_INTEGER) out_counts.push_back(na_count);
    else out_counts.push_back(counts[(size_t)((int64_t)v - (int64_t)minv)]);
  }

  std::vector<int> key_cols{by};
  SEXP keys = PROTECT(build_frame(df, first, key_cols));
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_VECTOR_ELT(out, 0, VECTOR_ELT(keys, 0));
  SET_STRING_ELT(names, 0, STRING_ELT(Rf_getAttrib(keys, R_NamesSymbol), 0));
  SEXP ncol = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t)out_counts.size()));
  for (R_xlen_t i = 0; i < (R_xlen_t)out_counts.size(); ++i) INTEGER(ncol)[i] = out_counts[(size_t)i];
  SET_VECTOR_ELT(out, 1, ncol);
  SET_STRING_ELT(names, 1, STRING_ELT(s_name, 0));
  Rf_setAttrib(out, R_NamesSymbol, names);
  Rf_setAttrib(out, R_RowNamesSymbol, make_row_names((R_xlen_t)out_counts.size()));
  set_table_class(out);
  UNPROTECT(4);
  *out_ptr = out;
  return true;
}

template <typename T>
bool count_hash_typed(SEXP df, int by, const T* p, R_xlen_t nrow, SEXP s_name, SEXP* out_ptr) {
  std::unordered_map<T, int> pos;
  pos.reserve((size_t)nrow);
  std::vector<R_xlen_t> first;
  std::vector<int> counts;
  for (R_xlen_t i = 0; i < nrow; ++i) {
    auto it = pos.find(p[i]);
    if (it == pos.end()) {
      int k = (int)first.size();
      pos.emplace(p[i], k);
      first.push_back(i);
      counts.push_back(1);
    } else {
      counts[(size_t)it->second]++;
    }
  }

  std::vector<int> key_cols{by};
  SEXP keys = PROTECT(build_frame(df, first, key_cols));
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_VECTOR_ELT(out, 0, VECTOR_ELT(keys, 0));
  SET_STRING_ELT(names, 0, STRING_ELT(Rf_getAttrib(keys, R_NamesSymbol), 0));
  SEXP ncol = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t)counts.size()));
  for (R_xlen_t i = 0; i < (R_xlen_t)counts.size(); ++i) INTEGER(ncol)[i] = counts[(size_t)i];
  SET_VECTOR_ELT(out, 1, ncol);
  SET_STRING_ELT(names, 1, STRING_ELT(s_name, 0));
  Rf_setAttrib(out, R_NamesSymbol, names);
  Rf_setAttrib(out, R_RowNamesSymbol, make_row_names((R_xlen_t)counts.size()));
  set_table_class(out);
  UNPROTECT(4);
  *out_ptr = out;
  return true;
}

bool count_single(SEXP df, int by, R_xlen_t nrow, SEXP s_name, SEXP* out) {
  SEXP col = VECTOR_ELT(df, by);
  switch (TYPEOF(col)) {
    case LGLSXP:
      return count_int_dense(df, by, LOGICAL(col), nrow, s_name, out) ||
             count_hash_typed<int>(df, by, LOGICAL(col), nrow, s_name, out);
    case INTSXP:
      return count_int_dense(df, by, INTEGER(col), nrow, s_name, out) ||
             count_hash_typed<int>(df, by, INTEGER(col), nrow, s_name, out);
    default:
      return false;
  }
}

bool group_agg_int_single(SEXP df, int by, const std::vector<int>& val, AggFun fun,
                          bool na_rm, SEXP* out_ptr) {
  Frame f = frame_from(df);
  SEXP key_col = VECTOR_ELT(df, by);
  if (TYPEOF(key_col) != INTSXP && TYPEOF(key_col) != LGLSXP) return false;
  const int* key = TYPEOF(key_col) == INTSXP ? INTEGER(key_col) : LOGICAL(key_col);
  const int nv = (int)val.size();

  int minv = INT_MAX, maxv = INT_MIN;
  for (R_xlen_t i = 0; i < f.nrow; ++i) {
    int k = key[i];
    if (k == NA_INTEGER) continue;
    if (k < minv) minv = k;
    if (k > maxv) maxv = k;
  }
  if (minv != INT_MAX) {
    uint64_t span = (uint64_t)((int64_t)maxv - (int64_t)minv + 1);
    if (span <= (uint64_t)f.nrow * 4 && span <= 10000000ULL) {
      if (nv == 1 && (fun == AGG_MEAN || fun == AGG_SUM)) {
        SEXP vcol = VECTOR_ELT(df, val[0]);
        if (TYPEOF(vcol) == REALSXP || TYPEOF(vcol) == INTSXP || TYPEOF(vcol) == LGLSXP) {
          std::vector<int> group_for((size_t)span, -1);
          int na_group = -1;
          std::vector<R_xlen_t> first;
          std::vector<double> sums;
          std::vector<int> counts;
          std::vector<char> bad;
          first.reserve((size_t)std::min<R_xlen_t>(f.nrow, (R_xlen_t)span + 1));

          for (R_xlen_t i = 0; i < f.nrow; ++i) {
            int g;
            int k = key[i];
            if (k == NA_INTEGER) {
              if (na_group < 0) {
                na_group = (int)first.size();
                first.push_back(i);
                sums.push_back(0.0);
                counts.push_back(0);
                bad.push_back(0);
              }
              g = na_group;
            } else {
              size_t slot = (size_t)((int64_t)k - (int64_t)minv);
              if (group_for[slot] < 0) {
                group_for[slot] = (int)first.size();
                first.push_back(i);
                sums.push_back(0.0);
                counts.push_back(0);
                bad.push_back(0);
              }
              g = group_for[slot];
            }

            bool na = false;
            double x = value_as_double(vcol, i, na);
            if (na) {
              if (!na_rm) bad[(size_t)g] = 1;
              continue;
            }
            sums[(size_t)g] += x;
            ++counts[(size_t)g];
          }

          std::vector<int> by_cols{by};
          SEXP keys = PROTECT(build_frame(df, first, by_cols));
          SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
          SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
          SET_VECTOR_ELT(result, 0, VECTOR_ELT(keys, 0));
          SET_STRING_ELT(names, 0, STRING_ELT(Rf_getAttrib(keys, R_NamesSymbol), 0));
          SEXP out_col = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)first.size()));
          for (R_xlen_t g = 0; g < (R_xlen_t)first.size(); ++g) {
            if (!na_rm && bad[(size_t)g]) REAL(out_col)[g] = NA_REAL;
            else if (fun == AGG_MEAN) REAL(out_col)[g] = counts[(size_t)g] > 0 ? sums[(size_t)g] / counts[(size_t)g] : NA_REAL;
            else REAL(out_col)[g] = sums[(size_t)g];
          }
          SET_VECTOR_ELT(result, 1, out_col);
          SET_STRING_ELT(names, 1, STRING_ELT(Rf_getAttrib(df, R_NamesSymbol), val[0]));
          Rf_setAttrib(result, R_NamesSymbol, names);
          Rf_setAttrib(result, R_RowNamesSymbol, make_row_names((R_xlen_t)first.size()));
          set_table_class(result);
          UNPROTECT(4);
          *out_ptr = result;
          return true;
        }
      }
      std::vector<int> group_for((size_t)span, -1);
      int na_group = -1;
      std::vector<R_xlen_t> first;
      std::vector<AggState> state;
      first.reserve((size_t)std::min<R_xlen_t>(f.nrow, (R_xlen_t)span + 1));

      for (R_xlen_t i = 0; i < f.nrow; ++i) {
        int g;
        int k = key[i];
        if (k == NA_INTEGER) {
          if (na_group < 0) {
            na_group = (int)first.size();
            first.push_back(i);
            state.resize(state.size() + nv);
          }
          g = na_group;
        } else {
          size_t slot = (size_t)((int64_t)k - (int64_t)minv);
          if (group_for[slot] < 0) {
            group_for[slot] = (int)first.size();
            first.push_back(i);
            state.resize(state.size() + nv);
          }
          g = group_for[slot];
        }

        for (int j = 0; j < nv; ++j) {
          AggState& s = state[(size_t)g * nv + j];
          if (fun == AGG_N) {
            ++s.n;
            continue;
          }
          bool na = false;
          double x = value_as_double(VECTOR_ELT(df, val[(size_t)j]), i, na);
          if (na) {
            if (!na_rm) s.bad = true;
            continue;
          }
          agg_update(s, fun, x);
        }
      }

      std::vector<int> by_cols{by};
      SEXP keys = PROTECT(build_frame(df, first, by_cols));
      SEXP result = PROTECT(Rf_allocVector(VECSXP, 1 + nv));
      SEXP names = PROTECT(Rf_allocVector(STRSXP, 1 + nv));
      SET_VECTOR_ELT(result, 0, VECTOR_ELT(keys, 0));
      SET_STRING_ELT(names, 0, STRING_ELT(Rf_getAttrib(keys, R_NamesSymbol), 0));

      SEXP df_names = Rf_getAttrib(df, R_NamesSymbol);
      for (int j = 0; j < nv; ++j) {
        SEXP col = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)first.size()));
        double* p = REAL(col);
        for (R_xlen_t g = 0; g < (R_xlen_t)first.size(); ++g)
          p[g] = agg_finish(state[(size_t)g * nv + j], fun, na_rm);
        SET_VECTOR_ELT(result, 1 + j, col);
        SET_STRING_ELT(names, 1 + j, STRING_ELT(df_names, val[(size_t)j]));
        UNPROTECT(1);
      }

      Rf_setAttrib(result, R_NamesSymbol, names);
      Rf_setAttrib(result, R_RowNamesSymbol, make_row_names((R_xlen_t)first.size()));
      set_table_class(result);
      UNPROTECT(3);
      *out_ptr = result;
      return true;
    }
  }

  std::unordered_map<int, int> pos;
  pos.reserve((size_t)f.nrow);
  std::vector<R_xlen_t> first;
  std::vector<AggState> state;

  for (R_xlen_t i = 0; i < f.nrow; ++i) {
    int k = key[i];
    auto it = pos.find(k);
    int g;
    if (it == pos.end()) {
      g = (int)first.size();
      pos.emplace(k, g);
      first.push_back(i);
      state.resize(state.size() + nv);
    } else {
      g = it->second;
    }

    for (int j = 0; j < nv; ++j) {
      AggState& s = state[(size_t)g * nv + j];
      if (fun == AGG_N) {
        ++s.n;
        continue;
      }
      bool na = false;
      double x = value_as_double(VECTOR_ELT(df, val[(size_t)j]), i, na);
      if (na) {
        if (!na_rm) s.bad = true;
        continue;
      }
      agg_update(s, fun, x);
    }
  }

  std::vector<int> by_cols{by};
  SEXP keys = PROTECT(build_frame(df, first, by_cols));
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 1 + nv));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 1 + nv));
  SET_VECTOR_ELT(result, 0, VECTOR_ELT(keys, 0));
  SET_STRING_ELT(names, 0, STRING_ELT(Rf_getAttrib(keys, R_NamesSymbol), 0));

  SEXP df_names = Rf_getAttrib(df, R_NamesSymbol);
  for (int j = 0; j < nv; ++j) {
    SEXP col = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)first.size()));
    double* p = REAL(col);
    for (R_xlen_t g = 0; g < (R_xlen_t)first.size(); ++g)
      p[g] = agg_finish(state[(size_t)g * nv + j], fun, na_rm);
    SET_VECTOR_ELT(result, 1 + j, col);
    SET_STRING_ELT(names, 1 + j, STRING_ELT(df_names, val[(size_t)j]));
    UNPROTECT(1);
  }

  Rf_setAttrib(result, R_NamesSymbol, names);
  Rf_setAttrib(result, R_RowNamesSymbol, make_row_names((R_xlen_t)first.size()));
  set_table_class(result);
  UNPROTECT(3);
  *out_ptr = result;
  return true;
}

bool match_mask_int_single(SEXP x, SEXP y, int x_by, int y_by, SEXP out) {
  SEXP xc = VECTOR_ELT(x, x_by);
  SEXP yc = VECTOR_ELT(y, y_by);
  if ((TYPEOF(xc) != INTSXP && TYPEOF(xc) != LGLSXP) ||
      (TYPEOF(yc) != INTSXP && TYPEOF(yc) != LGLSXP)) return false;
  Frame xf = frame_from(x);
  Frame yf = frame_from(y);
  const int* xp = TYPEOF(xc) == INTSXP ? INTEGER(xc) : LOGICAL(xc);
  const int* yp = TYPEOF(yc) == INTSXP ? INTEGER(yc) : LOGICAL(yc);
  std::unordered_set<int> keys;
  keys.reserve((size_t)yf.nrow);
  for (R_xlen_t i = 0; i < yf.nrow; ++i) keys.emplace(yp[i]);
  int* p = LOGICAL(out);
  for (R_xlen_t i = 0; i < xf.nrow; ++i)
    p[i] = keys.find(xp[i]) == keys.end() ? FALSE : TRUE;
  return true;
}

// Per-row integer rank of a character column in strcmp order, so ordering can
// compare ints instead of running strcmp at every comparison. Equal content
// shares a rank; NA rows sort first (!na_last) or last (na_last).
std::vector<int> str_rank(SEXP col, R_xlen_t nrow, bool na_last) {
  std::unordered_map<const void*, int> code;
  std::vector<SEXP> distinct;
  code.reserve((size_t)nrow);
  for (R_xlen_t i = 0; i < nrow; ++i) {
    SEXP s = STRING_ELT(col, i);
    if (s == NA_STRING) continue;
    if (code.emplace((const void*)s, 0).second) distinct.push_back(s);
  }
  std::sort(distinct.begin(), distinct.end(), [](SEXP a, SEXP b) {
    return std::strcmp(CHAR(a), CHAR(b)) < 0;
  });
  for (size_t r = 0; r < distinct.size(); ++r) {
    int rr = (r > 0 && std::strcmp(CHAR(distinct[r - 1]), CHAR(distinct[r])) == 0)
      ? code[(const void*)distinct[r - 1]]
      : (int)r + 1;
    code[(const void*)distinct[r]] = rr;
  }
  std::vector<int> rank((size_t)nrow);
  int na_rank = na_last ? INT_MAX : INT_MIN;
  for (R_xlen_t i = 0; i < nrow; ++i) {
    SEXP s = STRING_ELT(col, i);
    rank[(size_t)i] = (s == NA_STRING) ? na_rank : code[(const void*)s];
  }
  return rank;
}

int cmp_value(SEXP col, R_xlen_t a, R_xlen_t b, bool na_last) {
  switch (TYPEOF(col)) {
    case LGLSXP: {
      int x = LOGICAL(col)[a], y = LOGICAL(col)[b];
      bool xna = x == NA_LOGICAL, yna = y == NA_LOGICAL;
      if (xna || yna) {
        if (xna && yna) return 0;
        return (xna == na_last) ? 1 : -1;
      }
      return (x > y) - (x < y);
    }
    case INTSXP: {
      int x = INTEGER(col)[a], y = INTEGER(col)[b];
      bool xna = x == NA_INTEGER, yna = y == NA_INTEGER;
      if (xna || yna) {
        if (xna && yna) return 0;
        return (xna == na_last) ? 1 : -1;
      }
      return (x > y) - (x < y);
    }
    case REALSXP: {
      double x = REAL(col)[a], y = REAL(col)[b];
      bool xna = ISNAN(x), yna = ISNAN(y);
      if (xna || yna) {
        if (xna && yna) return 0;
        return (xna == na_last) ? 1 : -1;
      }
      return (x > y) - (x < y);
    }
    case STRSXP: {
      SEXP xs = STRING_ELT(col, a), ys = STRING_ELT(col, b);
      bool xna = xs == NA_STRING, yna = ys == NA_STRING;
      if (xna || yna) {
        if (xna && yna) return 0;
        return (xna == na_last) ? 1 : -1;
      }
      int c = std::strcmp(CHAR(xs), CHAR(ys));
      return (c > 0) - (c < 0);
    }
    default:
      Rf_error("basetable: unsupported order column type '%s'", Rf_type2char(TYPEOF(col)));
  }
}

// ---- join kernels ---------------------------------------------------------

void set_na_elt(SEXP col, R_xlen_t i) {
  switch (TYPEOF(col)) {
    case LGLSXP: LOGICAL(col)[i] = NA_LOGICAL; break;
    case INTSXP: INTEGER(col)[i] = NA_INTEGER; break;
    case REALSXP: REAL(col)[i] = NA_REAL; break;
    case CPLXSXP: { Rcomplex z; z.r = NA_REAL; z.i = NA_REAL; COMPLEX(col)[i] = z; break; }
    case STRSXP: SET_STRING_ELT(col, i, NA_STRING); break;
    case VECSXP: SET_VECTOR_ELT(col, i, R_NilValue); break;
    default:
      Rf_error("basetable: unsupported column type '%s'", Rf_type2char(TYPEOF(col)));
  }
}

void copy_elt(SEXP dst, R_xlen_t di, SEXP src, R_xlen_t si) {
  switch (TYPEOF(dst)) {
    case LGLSXP: LOGICAL(dst)[di] = LOGICAL(src)[si]; break;
    case INTSXP: INTEGER(dst)[di] = INTEGER(src)[si]; break;
    case REALSXP: REAL(dst)[di] = REAL(src)[si]; break;
    case CPLXSXP: COMPLEX(dst)[di] = COMPLEX(src)[si]; break;
    case STRSXP: SET_STRING_ELT(dst, di, STRING_ELT(src, si)); break;
    case VECSXP: SET_VECTOR_ELT(dst, di, VECTOR_ELT(src, si)); break;
    default:
      Rf_error("basetable: unsupported column type '%s'", Rf_type2char(TYPEOF(dst)));
  }
}

// Build one output column: element r is src[rows[r]], or NA when rows[r] < 0.
SEXP join_take(SEXP src, const std::vector<R_xlen_t>& rows) {
  R_xlen_t n = (R_xlen_t)rows.size();
  SEXP out = PROTECT(Rf_allocVector(TYPEOF(src), n));
  for (R_xlen_t i = 0; i < n; ++i) {
    R_xlen_t s = rows[(size_t)i];
    if (s < 0) set_na_elt(out, i);
    else copy_elt(out, i, src, s);
  }
  copy_common_attrs(out, src);
  UNPROTECT(1);
  return out;
}

// Merged key column: take from the x row when present, else the matching y row.
// When x and y key columns have different (numeric) types, the result is
// promoted to double, the way base::merge() coerces mismatched keys.
SEXP join_take_key(SEXP xc, SEXP yc,
                   const std::vector<R_xlen_t>& xrows,
                   const std::vector<R_xlen_t>& yrows) {
  R_xlen_t n = (R_xlen_t)xrows.size();

  bool x_str = Rf_isFactor(xc) || TYPEOF(xc) == STRSXP;
  bool y_str = Rf_isFactor(yc) || TYPEOF(yc) == STRSXP;
  bool same_factor = Rf_isFactor(xc) && Rf_isFactor(yc) &&
    R_compute_identical(Rf_getAttrib(xc, R_LevelsSymbol),
                        Rf_getAttrib(yc, R_LevelsSymbol), 0);

  if ((TYPEOF(xc) != TYPEOF(yc) || (Rf_isFactor(xc) && !same_factor)) && !same_factor) {
    if (key_is_numeric(xc) && key_is_numeric(yc)) {
      SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
      double* p = REAL(out);
      for (R_xlen_t i = 0; i < n; ++i) {
        int kind = 0;
        R_xlen_t xr = xrows[(size_t)i], yr = yrows[(size_t)i];
        if (xr >= 0) { double v = key_num(xc, xr, kind); p[i] = kind == 0 ? v : (kind == 2 ? R_NaN : NA_REAL); }
        else if (yr >= 0) { double v = key_num(yc, yr, kind); p[i] = kind == 0 ? v : (kind == 2 ? R_NaN : NA_REAL); }
        else p[i] = NA_REAL;
      }
      UNPROTECT(1);
      return out;
    }
    if (x_str && y_str) {
      SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
      for (R_xlen_t i = 0; i < n; ++i) {
        R_xlen_t xr = xrows[(size_t)i], yr = yrows[(size_t)i];
        if (xr >= 0) SET_STRING_ELT(out, i, key_str_elt(xc, xr));
        else if (yr >= 0) SET_STRING_ELT(out, i, key_str_elt(yc, yr));
        else SET_STRING_ELT(out, i, NA_STRING);
      }
      UNPROTECT(1);
      return out;
    }
    Rf_error("basetable: join key columns must share a type");
  }

  SEXP out = PROTECT(Rf_allocVector(TYPEOF(xc), n));
  for (R_xlen_t i = 0; i < n; ++i) {
    if (xrows[(size_t)i] >= 0) copy_elt(out, i, xc, xrows[(size_t)i]);
    else if (yrows[(size_t)i] >= 0) copy_elt(out, i, yc, yrows[(size_t)i]);
    else set_na_elt(out, i);
  }
  copy_common_attrs(out, xc);
  UNPROTECT(1);
  return out;
}

// Signed comparison of x[xi] against y[yi]: -1, 0, 1, or -2 when either is NA.
int pred_sign(SEXP xc, R_xlen_t xi, SEXP yc, R_xlen_t yi) {
  if (TYPEOF(xc) == STRSXP || TYPEOF(yc) == STRSXP) {
    if (TYPEOF(xc) != STRSXP || TYPEOF(yc) != STRSXP)
      Rf_error("basetable: cannot compare a character column with a non-character column");
    SEXP xs = STRING_ELT(xc, xi), ys = STRING_ELT(yc, yi);
    if (xs == NA_STRING || ys == NA_STRING) return -2;
    int c = std::strcmp(CHAR(xs), CHAR(ys));
    return (c > 0) - (c < 0);
  }
  bool na = false;
  double xv = value_as_double(xc, xi, na);
  if (na) return -2;
  double yv = value_as_double(yc, yi, na);
  if (na) return -2;
  return (xv > yv) - (xv < yv);
}

// Comparison op codes shared with the R layer.
enum CmpOp { CMP_LT = 0, CMP_LE = 1, CMP_GT = 2, CMP_GE = 3, CMP_EQ = 4 };

bool pred_ok(int sign, int op) {
  if (sign == -2) return false;
  switch (op) {
    case CMP_LT: return sign < 0;
    case CMP_LE: return sign <= 0;
    case CMP_GT: return sign > 0;
    case CMP_GE: return sign >= 0;
    case CMP_EQ: return sign == 0;
  }
  return false;
}

} // namespace

extern "C" SEXP bt_subset_(SEXP df, SEXP s_rows, SEXP s_cols) {
  Frame f = frame_from(df);
  std::vector<R_xlen_t> rows = row_index(s_rows, f.nrow);
  std::vector<int> cols = col_index(s_cols, f.ncol);
  return build_frame(df, rows, cols);
}

extern "C" SEXP bt_order_(SEXP df, SEXP s_by, SEXP s_decreasing, SEXP s_na_last) {
  Frame f = frame_from(df);
  std::vector<int> by = col_index(s_by, f.ncol);
  if (Rf_xlength(s_decreasing) != (R_xlen_t)by.size())
    Rf_error("basetable: decreasing length mismatch");
  bool na_last = Rf_asLogical(s_na_last) == TRUE;

  // Pre-rank character key columns so the comparator stays integer-only.
  std::vector<std::vector<int>> ranks(by.size());
  for (size_t k = 0; k < by.size(); ++k) {
    SEXP col = VECTOR_ELT(df, by[k]);
    if (TYPEOF(col) == STRSXP && !Rf_isFactor(col))
      ranks[k] = str_rank(col, f.nrow, na_last);
  }

  std::vector<R_xlen_t> ord;
  ord.reserve((size_t)f.nrow);
  for (R_xlen_t i = 0; i < f.nrow; ++i) ord.push_back(i);
  std::stable_sort(ord.begin(), ord.end(), [&](R_xlen_t a, R_xlen_t b) {
    for (size_t k = 0; k < by.size(); ++k) {
      int c;
      if (!ranks[k].empty()) {
        int ra = ranks[k][(size_t)a], rb = ranks[k][(size_t)b];
        c = (ra > rb) - (ra < rb);
      } else {
        c = cmp_value(VECTOR_ELT(df, by[k]), a, b, na_last);
      }
      if (c != 0) {
        bool dec = LOGICAL(s_decreasing)[k] == TRUE;
        return dec ? c > 0 : c < 0;
      }
    }
    return a < b;
  });
  std::vector<int> cols = col_index(R_NilValue, f.ncol);
  return build_frame(df, ord, cols);
}

extern "C" SEXP bt_unique_(SEXP df, SEXP s_by, SEXP s_keep_all) {
  Frame f = frame_from(df);
  std::vector<int> by = col_index(s_by, f.ncol);
  bool keep_all = Rf_asLogical(s_keep_all) == TRUE;
  std::vector<R_xlen_t> rows;
  std::vector<int> codes;
  if (by.size() == 1 && unique_single(VECTOR_ELT(df, by[0]), f.nrow, rows)) {
    // dense integer fast path filled `rows`
  } else if (by.size() == 1 && group_single(VECTOR_ELT(df, by[0]), f.nrow, codes, rows)) {
    // group_single filled `rows` with the first index per distinct value
  } else {
    KeyCodec codec(df, by);
    std::unordered_set<KeyBuf, KeyHash> seen;
    seen.reserve((size_t)f.nrow);
    rows.reserve((size_t)f.nrow);
    KeyBuf buf;
    for (R_xlen_t i = 0; i < f.nrow; ++i) {
      codec.encode(df, by, i, buf);
      if (seen.insert(buf).second) rows.push_back(i);
    }
  }
  std::vector<int> cols = keep_all ? col_index(R_NilValue, f.ncol) : by;
  return build_frame(df, rows, cols);
}

extern "C" SEXP bt_duplicated_(SEXP df, SEXP s_by, SEXP s_from_last) {
  Frame f = frame_from(df);
  std::vector<int> by = col_index(s_by, f.ncol);
  bool from_last = Rf_asLogical(s_from_last) == TRUE;
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, f.nrow));
  if (by.size() != 1 || !duplicated_single(VECTOR_ELT(df, by[0]), f.nrow, from_last, out)) {
    int* p = LOGICAL(out);
    std::fill(p, p + f.nrow, FALSE);
    KeyCodec codec(df, by);
    std::unordered_set<KeyBuf, KeyHash> seen;
    seen.reserve((size_t)f.nrow);
    KeyBuf buf;
    if (from_last) {
      for (R_xlen_t i = f.nrow; i-- > 0;) {
        codec.encode(df, by, i, buf);
        p[i] = seen.insert(buf).second ? FALSE : TRUE;
      }
    } else {
      for (R_xlen_t i = 0; i < f.nrow; ++i) {
        codec.encode(df, by, i, buf);
        p[i] = seen.insert(buf).second ? FALSE : TRUE;
      }
    }
  }
  UNPROTECT(1);
  return out;
}

extern "C" SEXP bt_count_(SEXP df, SEXP s_by, SEXP s_name) {
  Frame f = frame_from(df);
  std::vector<int> by = col_index(s_by, f.ncol);
  if (by.size() == 1) {
    SEXP out = R_NilValue;
    if (count_single(df, by[0], f.nrow, s_name, &out)) return out;
  }
  std::vector<R_xlen_t> first;
  std::vector<int> counts;
  std::vector<int> codes;
  if (by.size() == 1 && group_single(VECTOR_ELT(df, by[0]), f.nrow, codes, first)) {
    counts.assign(first.size(), 0);
    for (R_xlen_t i = 0; i < f.nrow; ++i) counts[(size_t)codes[(size_t)i]]++;
  } else {
    KeyCodec codec(df, by);
    std::unordered_map<KeyBuf, int, KeyHash> pos;
    pos.reserve((size_t)f.nrow);
    KeyBuf buf;
    for (R_xlen_t i = 0; i < f.nrow; ++i) {
      codec.encode(df, by, i, buf);
      auto it = pos.find(buf);
      if (it == pos.end()) {
        int p = (int)first.size();
        pos.emplace(buf, p);
        first.push_back(i);
        counts.push_back(1);
      } else {
        counts[(size_t)it->second]++;
      }
    }
  }
  SEXP out = PROTECT(build_frame(df, first, by));
  R_xlen_t nkey = Rf_xlength(out);
  SEXP with_n = PROTECT(Rf_allocVector(VECSXP, nkey + 1));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, nkey + 1));
  SEXP old_names = Rf_getAttrib(out, R_NamesSymbol);
  for (R_xlen_t j = 0; j < nkey; ++j) {
    SET_VECTOR_ELT(with_n, j, VECTOR_ELT(out, j));
    SET_STRING_ELT(names, j, STRING_ELT(old_names, j));
  }
  SEXP ncol = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t)counts.size()));
  for (R_xlen_t i = 0; i < (R_xlen_t)counts.size(); ++i) INTEGER(ncol)[i] = counts[(size_t)i];
  SET_VECTOR_ELT(with_n, nkey, ncol);
  SET_STRING_ELT(names, nkey, STRING_ELT(s_name, 0));
  Rf_setAttrib(with_n, R_NamesSymbol, names);
  Rf_setAttrib(with_n, R_RowNamesSymbol, make_row_names((R_xlen_t)counts.size()));
  set_table_class(with_n);
  UNPROTECT(4);
  return with_n;
}

extern "C" SEXP bt_group_agg_(SEXP df, SEXP s_by, SEXP s_value, SEXP s_fun, SEXP s_na_rm,
                              SEXP s_n_threads) {
  Frame f = frame_from(df);
  std::vector<int> by = col_index(s_by, f.ncol);
  std::vector<int> val = col_index(s_value, f.ncol);
  AggFun fun;
  if (!parse_agg_fun(s_fun, fun))
    Rf_error("basetable: unsupported aggregate function for native engine");
  bool na_rm = Rf_asLogical(s_na_rm) == TRUE;

  if (by.size() == 1) {
    SEXP fast = R_NilValue;
    if (group_agg_int_single(df, by[0], val, fun, na_rm, &fast)) return fast;
  }

  std::vector<R_xlen_t> first;
  std::vector<AggState> state;
  const int nv = (int)val.size();

  std::vector<int> codes;
  bool have_codes = by.size() == 1 && group_single(VECTOR_ELT(df, by[0]), f.nrow, codes, first);

  std::vector<SEXP> vcols((size_t)nv);
  for (int j = 0; j < nv; ++j) {
    vcols[(size_t)j] = VECTOR_ELT(df, val[(size_t)j]);
    int t = TYPEOF(vcols[(size_t)j]);
    if (fun != AGG_N && t != LGLSXP && t != INTSXP && t != REALSXP)
      Rf_error("basetable: aggregate value columns must be numeric, integer, or logical");
  }

  int nth = have_codes ? clamp_threads(s_n_threads, f.nrow, 750000) : 1;

  if (have_codes && nth > 1) {
    // Dense group codes: reduce row ranges into per-thread private accumulators,
    // then merge. No R API calls in the parallel section.
    size_t width = first.size() * (size_t)nv;
    std::vector<std::vector<AggState>> partial((size_t)nth, std::vector<AggState>(width));
    auto worker = [&](int t, R_xlen_t lo, R_xlen_t hi) {
      std::vector<AggState>& st = partial[(size_t)t];
      for (R_xlen_t i = lo; i < hi; ++i) {
        size_t base = (size_t)codes[(size_t)i] * (size_t)nv;
        for (int j = 0; j < nv; ++j) {
          AggState& s = st[base + (size_t)j];
          if (fun == AGG_N) { ++s.n; continue; }
          bool na = false;
          double x = value_as_double(vcols[(size_t)j], i, na);
          if (na) { if (!na_rm) s.bad = true; continue; }
          agg_update(s, fun, x);
        }
      }
    };
    std::vector<std::thread> pool;
    R_xlen_t chunk = (f.nrow + nth - 1) / nth;
    for (int t = 0; t < nth; ++t) {
      R_xlen_t lo = (R_xlen_t)t * chunk, hi = std::min<R_xlen_t>(f.nrow, lo + chunk);
      if (lo >= hi) break;
      pool.emplace_back(worker, t, lo, hi);
    }
    for (auto& th : pool) th.join();
    state.assign(width, AggState());
    for (int t = 0; t < nth; ++t)
      for (size_t k = 0; k < width; ++k)
        agg_merge(state[k], partial[(size_t)t][k]);
  } else {
    if (have_codes) state.resize(first.size() * (size_t)nv);
    KeyCodec codec(df, by);
    std::unordered_map<KeyBuf, int, KeyHash> pos;
    KeyBuf buf;
    if (!have_codes) pos.reserve((size_t)f.nrow);
    for (R_xlen_t i = 0; i < f.nrow; ++i) {
      int g;
      if (have_codes) {
        g = codes[(size_t)i];
      } else {
        codec.encode(df, by, i, buf);
        auto it = pos.find(buf);
        if (it == pos.end()) {
          g = (int)first.size();
          pos.emplace(buf, g);
          first.push_back(i);
          state.resize(state.size() + nv);
        } else {
          g = it->second;
        }
      }
      for (int j = 0; j < nv; ++j) {
        AggState& s = state[(size_t)g * nv + j];
        if (fun == AGG_N) { ++s.n; continue; }
        bool na = false;
        double x = value_as_double(vcols[(size_t)j], i, na);
        if (na) { if (!na_rm) s.bad = true; continue; }
        agg_update(s, fun, x);
      }
    }
  }

  SEXP out = PROTECT(build_frame(df, first, by));
  R_xlen_t nk = Rf_xlength(out);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, nk + nv));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, nk + nv));
  SEXP key_names = Rf_getAttrib(out, R_NamesSymbol);
  for (R_xlen_t j = 0; j < nk; ++j) {
    SET_VECTOR_ELT(result, j, VECTOR_ELT(out, j));
    SET_STRING_ELT(names, j, STRING_ELT(key_names, j));
  }

  SEXP df_names = Rf_getAttrib(df, R_NamesSymbol);
  for (int j = 0; j < nv; ++j) {
    SEXP col = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)first.size()));
    double* p = REAL(col);
    for (R_xlen_t g = 0; g < (R_xlen_t)first.size(); ++g) {
      p[g] = agg_finish(state[(size_t)g * nv + j], fun, na_rm);
    }
    SET_VECTOR_ELT(result, nk + j, col);
    SET_STRING_ELT(names, nk + j, STRING_ELT(df_names, val[(size_t)j]));
    UNPROTECT(1);
  }

  Rf_setAttrib(result, R_NamesSymbol, names);
  Rf_setAttrib(result, R_RowNamesSymbol, make_row_names((R_xlen_t)first.size()));
  set_table_class(result);
  UNPROTECT(3);
  return result;
}

extern "C" SEXP bt_match_mask_(SEXP x, SEXP y, SEXP s_x_by, SEXP s_y_by) {
  Frame xf = frame_from(x);
  Frame yf = frame_from(y);
  std::vector<int> x_by = col_index(s_x_by, xf.ncol);
  std::vector<int> y_by = col_index(s_y_by, yf.ncol);
  if (x_by.size() != y_by.size())
    Rf_error("basetable: join key length mismatch");

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, xf.nrow));
  if (x_by.size() == 1 && match_mask_int_single(x, y, x_by[0], y_by[0], out)) {
    UNPROTECT(1);
    return out;
  }

  KeyCodec codec(y, y_by);
  codec.unify(x, x_by);
  std::unordered_set<KeyBuf, KeyHash> keys;
  keys.reserve((size_t)yf.nrow);
  KeyBuf buf;
  for (R_xlen_t j = 0; j < yf.nrow; ++j) {
    codec.encode(y, y_by, j, buf);
    keys.insert(buf);
  }
  codec.freeze();
  int* p = LOGICAL(out);
  for (R_xlen_t i = 0; i < xf.nrow; ++i) {
    codec.encode(x, x_by, i, buf);
    p[i] = keys.find(buf) == keys.end() ? FALSE : TRUE;
  }
  UNPROTECT(1);
  return out;
}

extern "C" SEXP bt_group_id_(SEXP df, SEXP s_by) {
  Frame f = frame_from(df);
  std::vector<int> by = col_index(s_by, f.ncol);
  SEXP ids = PROTECT(Rf_allocVector(INTSXP, f.nrow));
  std::vector<R_xlen_t> first;
  std::vector<int> codes;

  if (by.size() == 1 && group_single(VECTOR_ELT(df, by[0]), f.nrow, codes, first)) {
    for (R_xlen_t i = 0; i < f.nrow; ++i) INTEGER(ids)[i] = codes[(size_t)i] + 1;
    for (size_t g = 0; g < first.size(); ++g) first[g] += 1;  // to 1-based row index
  } else {
    KeyCodec codec(df, by);
    std::unordered_map<KeyBuf, int, KeyHash> pos;
    pos.reserve((size_t)f.nrow);
    KeyBuf buf;
    for (R_xlen_t i = 0; i < f.nrow; ++i) {
      codec.encode(df, by, i, buf);
      auto it = pos.find(buf);
      int g;
      if (it == pos.end()) {
        g = (int)first.size() + 1;
        pos.emplace(buf, g);
        first.push_back(i + 1);
      } else {
        g = it->second;
      }
      INTEGER(ids)[i] = g;
    }
  }

  SEXP firsts = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t)first.size()));
  for (R_xlen_t i = 0; i < (R_xlen_t)first.size(); ++i) {
    INTEGER(firsts)[i] = (int)first[(size_t)i];
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_VECTOR_ELT(out, 0, ids);
  SET_VECTOR_ELT(out, 1, firsts);
  SET_STRING_ELT(names, 0, Rf_mkChar("id"));
  SET_STRING_ELT(names, 1, Rf_mkChar("first"));
  Rf_setAttrib(out, R_NamesSymbol, names);
  UNPROTECT(4);
  return out;
}

// Full equi-join materialisation. Column layout mirrors data.table's
// merge.data.table: join keys first (named after x), then x's remaining
// columns, then y's remaining columns, with `suffixes` applied to names that
// collide between the two non-key sets. An empty key set produces the
// Cartesian product.
extern "C" SEXP bt_join_(SEXP x, SEXP y, SEXP s_x_by, SEXP s_y_by,
                         SEXP s_all_x, SEXP s_all_y, SEXP s_suffixes) {
  Frame xf = frame_from(x);
  Frame yf = frame_from(y);
  std::vector<int> x_by = col_index(s_x_by, xf.ncol);
  std::vector<int> y_by = col_index(s_y_by, yf.ncol);
  if (x_by.size() != y_by.size())
    Rf_error("basetable: join key length mismatch");
  bool all_x = Rf_asLogical(s_all_x) == TRUE;
  bool all_y = Rf_asLogical(s_all_y) == TRUE;

  std::vector<char> x_is_key((size_t)xf.ncol, 0), y_is_key((size_t)yf.ncol, 0);
  for (int c : x_by) x_is_key[(size_t)c] = 1;
  for (int c : y_by) y_is_key[(size_t)c] = 1;
  std::vector<int> x_extra, y_extra;
  for (int j = 0; j < xf.ncol; ++j) if (!x_is_key[(size_t)j]) x_extra.push_back(j);
  for (int j = 0; j < yf.ncol; ++j) if (!y_is_key[(size_t)j]) y_extra.push_back(j);

  bool cross = x_by.empty();
  std::unordered_map<KeyBuf, std::vector<R_xlen_t>, KeyHash> ymap;
  std::vector<R_xlen_t> all_y_rows;
  KeyCodec codec(y, y_by);
  KeyBuf buf;
  if (cross) {
    all_y_rows.reserve((size_t)yf.nrow);
    for (R_xlen_t j = 0; j < yf.nrow; ++j) all_y_rows.push_back(j);
  } else {
    codec.unify(x, x_by);
    ymap.reserve((size_t)yf.nrow);
    for (R_xlen_t j = 0; j < yf.nrow; ++j) {
      codec.encode(y, y_by, j, buf);
      ymap[buf].push_back(j);
    }
    codec.freeze();
  }

  std::vector<R_xlen_t> xrows, yrows;
  std::vector<char> y_matched(all_y ? (size_t)yf.nrow : 0, 0);
  for (R_xlen_t i = 0; i < xf.nrow; ++i) {
    const std::vector<R_xlen_t>* matches = nullptr;
    if (cross) {
      matches = &all_y_rows;
    } else {
      codec.encode(x, x_by, i, buf);
      auto it = ymap.find(buf);
      if (it != ymap.end()) matches = &it->second;
    }
    if (matches && !matches->empty()) {
      for (R_xlen_t yi : *matches) {
        xrows.push_back(i);
        yrows.push_back(yi);
        if (all_y) y_matched[(size_t)yi] = 1;
      }
    } else if (all_x) {
      xrows.push_back(i);
      yrows.push_back(-1);
    }
  }
  if (all_y) {
    for (R_xlen_t j = 0; j < yf.nrow; ++j)
      if (!y_matched[(size_t)j]) { xrows.push_back(-1); yrows.push_back(j); }
  }

  R_xlen_t nout = (R_xlen_t)xrows.size();
  R_xlen_t ncol_out = (R_xlen_t)(x_by.size() + x_extra.size() + y_extra.size());
  SEXP x_names = Rf_getAttrib(x, R_NamesSymbol);
  SEXP y_names = Rf_getAttrib(y, R_NamesSymbol);

  std::unordered_set<std::string> x_extra_names;
  for (int c : x_extra) x_extra_names.insert(CHAR(STRING_ELT(x_names, c)));
  std::unordered_set<std::string> dup;
  for (int c : y_extra) {
    std::string nm = CHAR(STRING_ELT(y_names, c));
    if (x_extra_names.count(nm)) dup.insert(nm);
  }
  const char* sfx_x = ".x";
  const char* sfx_y = ".y";
  if (TYPEOF(s_suffixes) == STRSXP && Rf_xlength(s_suffixes) == 2) {
    sfx_x = CHAR(STRING_ELT(s_suffixes, 0));
    sfx_y = CHAR(STRING_ELT(s_suffixes, 1));
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, ncol_out));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, ncol_out));
  R_xlen_t p = 0;
  for (size_t k = 0; k < x_by.size(); ++k) {
    SET_VECTOR_ELT(out, p, join_take_key(VECTOR_ELT(x, x_by[k]), VECTOR_ELT(y, y_by[k]), xrows, yrows));
    SET_STRING_ELT(names, p, STRING_ELT(x_names, x_by[k]));
    ++p;
  }
  for (int c : x_extra) {
    SET_VECTOR_ELT(out, p, join_take(VECTOR_ELT(x, c), xrows));
    std::string nm = CHAR(STRING_ELT(x_names, c));
    if (dup.count(nm)) { nm += sfx_x; SET_STRING_ELT(names, p, Rf_mkChar(nm.c_str())); }
    else SET_STRING_ELT(names, p, STRING_ELT(x_names, c));
    ++p;
  }
  for (int c : y_extra) {
    SET_VECTOR_ELT(out, p, join_take(VECTOR_ELT(y, c), yrows));
    std::string nm = CHAR(STRING_ELT(y_names, c));
    if (dup.count(nm)) { nm += sfx_y; SET_STRING_ELT(names, p, Rf_mkChar(nm.c_str())); }
    else SET_STRING_ELT(names, p, STRING_ELT(y_names, c));
    ++p;
  }
  Rf_setAttrib(out, R_NamesSymbol, names);
  Rf_setAttrib(out, R_RowNamesSymbol, make_row_names(nout));
  set_table_class(out);
  UNPROTECT(2);
  return out;
}

// First matching y row (1-based) for each x row on an equi key, NA when none.
extern "C" SEXP bt_first_match_(SEXP x, SEXP y, SEXP s_x_by, SEXP s_y_by) {
  Frame xf = frame_from(x);
  Frame yf = frame_from(y);
  std::vector<int> x_by = col_index(s_x_by, xf.ncol);
  std::vector<int> y_by = col_index(s_y_by, yf.ncol);
  if (x_by.size() != y_by.size())
    Rf_error("basetable: join key length mismatch");

  KeyCodec codec(y, y_by);
  codec.unify(x, x_by);
  std::unordered_map<KeyBuf, int, KeyHash> first;
  first.reserve((size_t)yf.nrow);
  KeyBuf buf;
  for (R_xlen_t j = 0; j < yf.nrow; ++j) {
    codec.encode(y, y_by, j, buf);
    first.emplace(buf, (int)(j + 1));
  }
  codec.freeze();

  SEXP out = PROTECT(Rf_allocVector(INTSXP, xf.nrow));
  int* p = INTEGER(out);
  for (R_xlen_t i = 0; i < xf.nrow; ++i) {
    codec.encode(x, x_by, i, buf);
    auto it = first.find(buf);
    p[i] = it == first.end() ? NA_INTEGER : it->second;
  }
  UNPROTECT(1);
  return out;
}

// Join on zero or more equi keys plus a list of (x col, op, y col) comparison
// predicates (op codes: 0 '<', 1 '<=', 2 '>', 3 '>=', 4 '=='). Output is every
// x column followed by the requested y columns; unmatched x rows are kept with
// NA y values when all_x is TRUE.
extern "C" SEXP bt_range_join_(SEXP x, SEXP y, SEXP s_x_by, SEXP s_y_by,
                               SEXP s_px, SEXP s_py, SEXP s_pop,
                               SEXP s_y_cols, SEXP s_all_x) {
  Frame xf = frame_from(x);
  Frame yf = frame_from(y);
  std::vector<int> x_by = col_index(s_x_by, xf.ncol);
  std::vector<int> y_by = col_index(s_y_by, yf.ncol);
  std::vector<int> px = col_index(s_px, xf.ncol);
  std::vector<int> py = col_index(s_py, yf.ncol);
  if (TYPEOF(s_pop) != INTSXP)
    Rf_error("basetable: predicate op codes must be integer");
  std::vector<int> pop(INTEGER(s_pop), INTEGER(s_pop) + Rf_xlength(s_pop));
  if (x_by.size() != y_by.size() || px.size() != py.size() || px.size() != pop.size())
    Rf_error("basetable: join predicate arity mismatch");
  std::vector<int> y_cols = col_index(s_y_cols, yf.ncol);
  bool all_x = Rf_asLogical(s_all_x) == TRUE;

  bool has_keys = !x_by.empty();
  std::unordered_map<KeyBuf, std::vector<R_xlen_t>, KeyHash> ymap;
  std::vector<R_xlen_t> all_y_rows;
  KeyCodec codec(y, y_by);
  KeyBuf buf;
  if (has_keys) {
    codec.unify(x, x_by);
    ymap.reserve((size_t)yf.nrow);
    for (R_xlen_t j = 0; j < yf.nrow; ++j) {
      codec.encode(y, y_by, j, buf);
      ymap[buf].push_back(j);
    }
    codec.freeze();
  } else {
    all_y_rows.reserve((size_t)yf.nrow);
    for (R_xlen_t j = 0; j < yf.nrow; ++j) all_y_rows.push_back(j);
  }

  std::vector<R_xlen_t> xrows, yrows;
  for (R_xlen_t i = 0; i < xf.nrow; ++i) {
    const std::vector<R_xlen_t>* bucket = nullptr;
    if (has_keys) {
      codec.encode(x, x_by, i, buf);
      auto it = ymap.find(buf);
      if (it != ymap.end()) bucket = &it->second;
    } else {
      bucket = &all_y_rows;
    }
    bool hit = false;
    if (bucket) {
      for (R_xlen_t j : *bucket) {
        bool ok = true;
        for (size_t k = 0; k < px.size(); ++k) {
          if (!pred_ok(pred_sign(VECTOR_ELT(x, px[k]), i, VECTOR_ELT(y, py[k]), j), pop[k])) {
            ok = false;
            break;
          }
        }
        if (ok) { xrows.push_back(i); yrows.push_back(j); hit = true; }
      }
    }
    if (!hit && all_x) { xrows.push_back(i); yrows.push_back(-1); }
  }

  R_xlen_t nout = (R_xlen_t)xrows.size();
  R_xlen_t nc = xf.ncol + (R_xlen_t)y_cols.size();
  SEXP xn = Rf_getAttrib(x, R_NamesSymbol);
  SEXP yn = Rf_getAttrib(y, R_NamesSymbol);
  SEXP out = PROTECT(Rf_allocVector(VECSXP, nc));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, nc));
  for (R_xlen_t j = 0; j < xf.ncol; ++j) {
    SET_VECTOR_ELT(out, j, join_take(VECTOR_ELT(x, j), xrows));
    SET_STRING_ELT(names, j, STRING_ELT(xn, j));
  }
  for (size_t k = 0; k < y_cols.size(); ++k) {
    SET_VECTOR_ELT(out, xf.ncol + (R_xlen_t)k, join_take(VECTOR_ELT(y, y_cols[k]), yrows));
    SET_STRING_ELT(names, xf.ncol + (R_xlen_t)k, STRING_ELT(yn, y_cols[k]));
  }
  Rf_setAttrib(out, R_NamesSymbol, names);
  Rf_setAttrib(out, R_RowNamesSymbol, make_row_names(nout));
  set_table_class(out);
  UNPROTECT(2);
  return out;
}

// Rolling join on zero or more exact keys plus one ordered "roll" key.
// direction: 0 backward (y <= x), 1 forward (y >= x), 2 nearest. For each x row
// the closest surviving y row within `tolerance` wins (first on ties). Every x
// row is kept; the requested y columns carry NA when nothing matched, and a y
// name that collides with an x name gets a ".y" suffix.
extern "C" SEXP bt_rolling_join_(SEXP x, SEXP y, SEXP s_x_exact, SEXP s_y_exact,
                                 SEXP s_x_roll, SEXP s_y_roll, SEXP s_dir,
                                 SEXP s_tol, SEXP s_y_cols) {
  Frame xf = frame_from(x);
  Frame yf = frame_from(y);
  std::vector<int> xe = col_index(s_x_exact, xf.ncol);
  std::vector<int> ye = col_index(s_y_exact, yf.ncol);
  if (xe.size() != ye.size())
    Rf_error("basetable: rolling join key length mismatch");
  int xr = Rf_asInteger(s_x_roll) - 1;
  int yr = Rf_asInteger(s_y_roll) - 1;
  if (xr < 0 || xr >= xf.ncol || yr < 0 || yr >= yf.ncol)
    Rf_error("basetable: rolling key out of bounds");
  int dir = Rf_asInteger(s_dir);
  double tol = Rf_asReal(s_tol);
  std::vector<int> y_cols = col_index(s_y_cols, yf.ncol);

  bool has_exact = !xe.empty();
  std::unordered_map<KeyBuf, std::vector<R_xlen_t>, KeyHash> ymap;
  std::vector<R_xlen_t> all_y_rows;
  KeyCodec codec(y, ye);
  KeyBuf buf;
  if (has_exact) {
    codec.unify(x, xe);
    ymap.reserve((size_t)yf.nrow);
    for (R_xlen_t j = 0; j < yf.nrow; ++j) {
      codec.encode(y, ye, j, buf);
      ymap[buf].push_back(j);
    }
    codec.freeze();
  } else {
    all_y_rows.reserve((size_t)yf.nrow);
    for (R_xlen_t j = 0; j < yf.nrow; ++j) all_y_rows.push_back(j);
  }

  SEXP xroll = VECTOR_ELT(x, xr);
  SEXP yroll = VECTOR_ELT(y, yr);
  std::vector<R_xlen_t> matchrow((size_t)xf.nrow, -1);
  for (R_xlen_t i = 0; i < xf.nrow; ++i) {
    const std::vector<R_xlen_t>* bucket = nullptr;
    if (has_exact) {
      codec.encode(x, xe, i, buf);
      auto it = ymap.find(buf);
      if (it != ymap.end()) bucket = &it->second;
    } else {
      bucket = &all_y_rows;
    }
    if (!bucket) continue;
    bool xna = false;
    double xv = value_as_double(xroll, i, xna);
    if (xna) continue;
    double best = R_PosInf;
    R_xlen_t bestj = -1;
    for (R_xlen_t j : *bucket) {
      bool yna = false;
      double yv = value_as_double(yroll, j, yna);
      if (yna) continue;
      double delta = yv - xv;
      bool okdir = dir == 0 ? delta <= 0 : (dir == 1 ? delta >= 0 : true);
      if (!okdir) continue;
      double ad = std::fabs(delta);
      if (ad > tol) continue;
      if (ad < best) { best = ad; bestj = j; }
    }
    matchrow[(size_t)i] = bestj;
  }

  std::unordered_set<std::string> x_name_set;
  SEXP xn = Rf_getAttrib(x, R_NamesSymbol);
  SEXP yn = Rf_getAttrib(y, R_NamesSymbol);
  for (R_xlen_t j = 0; j < xf.ncol; ++j) x_name_set.insert(CHAR(STRING_ELT(xn, j)));

  R_xlen_t nc = xf.ncol + (R_xlen_t)y_cols.size();
  SEXP out = PROTECT(Rf_allocVector(VECSXP, nc));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, nc));
  for (R_xlen_t j = 0; j < xf.ncol; ++j) {
    SET_VECTOR_ELT(out, j, VECTOR_ELT(x, j));
    SET_STRING_ELT(names, j, STRING_ELT(xn, j));
  }
  for (size_t k = 0; k < y_cols.size(); ++k) {
    SET_VECTOR_ELT(out, xf.ncol + (R_xlen_t)k, join_take(VECTOR_ELT(y, y_cols[k]), matchrow));
    std::string nm = CHAR(STRING_ELT(yn, y_cols[k]));
    if (x_name_set.count(nm)) { nm += ".y"; SET_STRING_ELT(names, xf.ncol + (R_xlen_t)k, Rf_mkChar(nm.c_str())); }
    else SET_STRING_ELT(names, xf.ncol + (R_xlen_t)k, STRING_ELT(yn, y_cols[k]));
  }
  Rf_setAttrib(out, R_NamesSymbol, names);
  Rf_setAttrib(out, R_RowNamesSymbol, make_row_names(xf.nrow));
  set_table_class(out);
  UNPROTECT(2);
  return out;
}

// Row-bind a list of data.frames. With fill = TRUE the column set is the union
// of all input names (first-seen order) and missing columns become NA; with
// fill = FALSE only the first frame's columns are used. A column present with
// different types across inputs is promoted along logical < integer < double <
// character (factors and any string force character). `id_name` (optional) adds
// a leading label column drawn from `id_values` (the names of the input list).
extern "C" SEXP bt_rbind_(SEXP frames, SEXP s_fill, SEXP s_id_name, SEXP s_id_values) {
  if (TYPEOF(frames) != VECSXP)
    Rf_error("basetable: bt_rbind_ expects a list of data.frames");
  R_xlen_t nf = Rf_xlength(frames);
  bool fill = Rf_asLogical(s_fill) == TRUE;
  const char* id_name = (TYPEOF(s_id_name) == STRSXP && Rf_xlength(s_id_name) == 1)
    ? CHAR(STRING_ELT(s_id_name, 0)) : nullptr;

  std::vector<std::string> col_names;
  std::unordered_map<std::string, int> col_pos;
  std::vector<R_xlen_t> nrows((size_t)nf, 0);
  R_xlen_t total = 0;

  for (R_xlen_t f = 0; f < nf; ++f) {
    SEXP df = VECTOR_ELT(frames, f);
    Frame fr = frame_from(df);
    nrows[(size_t)f] = fr.nrow;
    total += fr.nrow;
    SEXP nm = Rf_getAttrib(df, R_NamesSymbol);
    if (f == 0 || fill) {
      for (R_xlen_t j = 0; j < fr.ncol; ++j) {
        std::string s = CHAR(STRING_ELT(nm, j));
        if (col_pos.find(s) == col_pos.end()) {
          col_pos.emplace(s, (int)col_names.size());
          col_names.push_back(s);
        }
      }
    }
  }
  int ncol = (int)col_names.size();

  // Resolve the target type of each output column.
  auto rank = [](SEXPTYPE t) {
    switch (t) { case LGLSXP: return 0; case INTSXP: return 1; case REALSXP: return 2; default: return 3; }
  };
  std::vector<SEXPTYPE> out_type((size_t)ncol, LGLSXP);
  std::vector<char> seen((size_t)ncol, 0);
  for (R_xlen_t f = 0; f < nf; ++f) {
    SEXP df = VECTOR_ELT(frames, f);
    SEXP nm = Rf_getAttrib(df, R_NamesSymbol);
    for (R_xlen_t j = 0; j < Rf_xlength(df); ++j) {
      auto it = col_pos.find(CHAR(STRING_ELT(nm, j)));
      if (it == col_pos.end()) continue;
      SEXP col = VECTOR_ELT(df, j);
      SEXPTYPE t = Rf_isFactor(col) ? STRSXP : TYPEOF(col);
      if (t != LGLSXP && t != INTSXP && t != REALSXP && t != STRSXP) t = STRSXP;
      int c = it->second;
      out_type[(size_t)c] = seen[(size_t)c]
        ? (rank(t) > rank(out_type[(size_t)c]) ? t : out_type[(size_t)c])
        : t;
      seen[(size_t)c] = 1;
    }
  }

  int extra = id_name ? 1 : 0;
  SEXP out = PROTECT(Rf_allocVector(VECSXP, ncol + extra));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, ncol + extra));

  if (id_name) {
    SEXP idcol = PROTECT(Rf_allocVector(STRSXP, total));
    R_xlen_t at = 0;
    bool have_vals = TYPEOF(s_id_values) == STRSXP && Rf_xlength(s_id_values) == nf;
    for (R_xlen_t f = 0; f < nf; ++f) {
      SEXP lbl = have_vals ? STRING_ELT(s_id_values, f) : R_NilValue;
      for (R_xlen_t i = 0; i < nrows[(size_t)f]; ++i) {
        if (have_vals && lbl != NA_STRING && CHAR(lbl)[0] != '\0')
          SET_STRING_ELT(idcol, at++, lbl);
        else {
          char buf[24];
          std::snprintf(buf, sizeof(buf), "%lld", (long long)(f + 1));
          SET_STRING_ELT(idcol, at++, Rf_mkChar(buf));
        }
      }
    }
    SET_VECTOR_ELT(out, 0, idcol);
    SET_STRING_ELT(names, 0, Rf_mkChar(id_name));
    UNPROTECT(1);
  }

  for (int c = 0; c < ncol; ++c) {
    SEXPTYPE tt = out_type[(size_t)c];
    SEXP col = PROTECT(Rf_allocVector(tt, total));
    R_xlen_t at = 0;
    for (R_xlen_t f = 0; f < nf; ++f) {
      SEXP df = VECTOR_ELT(frames, f);
      SEXP nm = Rf_getAttrib(df, R_NamesSymbol);
      int src = -1;
      for (R_xlen_t j = 0; j < Rf_xlength(df); ++j)
        if (!std::strcmp(CHAR(STRING_ELT(nm, j)), col_names[(size_t)c].c_str())) { src = (int)j; break; }
      R_xlen_t rn = nrows[(size_t)f];
      if (src < 0) {
        for (R_xlen_t i = 0; i < rn; ++i) set_na_elt(col, at + i);
        at += rn;
        continue;
      }
      SEXP s = VECTOR_ELT(df, src);
      for (R_xlen_t i = 0; i < rn; ++i, ++at) {
        if (tt == STRSXP) {
          SET_STRING_ELT(col, at, key_str_or_coerce(s, i));
        } else if (tt == REALSXP) {
          int kind = 0;
          double v = key_is_numeric(s) ? key_num(s, i, kind) : (kind = 1, 0.0);
          REAL(col)[at] = kind == 0 ? v : (kind == 2 ? R_NaN : NA_REAL);
        } else if (tt == INTSXP) {
          if (TYPEOF(s) == INTSXP) INTEGER(col)[at] = INTEGER(s)[i];
          else if (TYPEOF(s) == LGLSXP) INTEGER(col)[at] = LOGICAL(s)[i];
          else INTEGER(col)[at] = NA_INTEGER;
        } else {  // LGLSXP
          LOGICAL(col)[at] = TYPEOF(s) == LGLSXP ? LOGICAL(s)[i] : NA_LOGICAL;
        }
      }
    }
    SET_VECTOR_ELT(out, c + extra, col);
    SET_STRING_ELT(names, c + extra, Rf_mkChar(col_names[(size_t)c].c_str()));
    UNPROTECT(1);
  }

  Rf_setAttrib(out, R_NamesSymbol, names);
  Rf_setAttrib(out, R_RowNamesSymbol, make_row_names(total));
  set_table_class(out);
  UNPROTECT(2);
  return out;
}

// ---- expression kernel --------------------------------------------------------
//
// A tiny stack machine that evaluates arithmetic / comparison / boolean
// expressions over columns in one pass, without allocating the R intermediates
// that `eval()` would. The R layer compiles a supported call tree to postfix
// bytecode and falls back to `eval()` for anything unsupported. Every value is
// carried as double; a per-slot `logical` flag decides whether the final result
// is returned as LGLSXP or REALSXP.

namespace {

enum ExprOp {
  EX_COL = 1, EX_CONST = 2, EX_TRUE = 3, EX_FALSE = 4, EX_NA = 5,
  EX_ADD = 10, EX_SUB = 11, EX_MUL = 12, EX_DIV = 13, EX_POW = 14, EX_MOD = 15,
  EX_LT = 20, EX_LE = 21, EX_GT = 22, EX_GE = 23, EX_EQ = 24, EX_NE = 25,
  EX_AND = 30, EX_OR = 31,
  EX_NEG = 40, EX_NOT = 41, EX_POS = 42,
  EX_IFELSE = 50
};

struct ExprVal {
  std::vector<double> data;  // size 1 (scalar) or n
  bool logical = false;
};

double col_as_double(SEXP col, R_xlen_t i) {
  switch (TYPEOF(col)) {
    case LGLSXP: { int v = LOGICAL(col)[i]; return v == NA_LOGICAL ? NA_REAL : (double)v; }
    case INTSXP: { int v = INTEGER(col)[i]; return v == NA_INTEGER ? NA_REAL : (double)v; }
    case REALSXP: return REAL(col)[i];
    default: Rf_error("basetable: expression columns must be numeric or logical");
  }
}

inline double bin_arith(int op, double a, double b) {
  if (ISNAN(a) || ISNAN(b)) return NA_REAL;
  switch (op) {
    case EX_ADD: return a + b;
    case EX_SUB: return a - b;
    case EX_MUL: return a * b;
    case EX_DIV: return a / b;
    case EX_POW: return std::pow(a, b);
    case EX_MOD: {
      if (b == 0.0) return R_NaN;
      double r = std::fmod(a, b);
      if (r != 0.0 && ((r < 0.0) != (b < 0.0))) r += b;
      return r;
    }
  }
  return NA_REAL;
}

inline double bin_cmp(int op, double a, double b) {
  if (ISNAN(a) || ISNAN(b)) return NA_REAL;
  switch (op) {
    case EX_LT: return a < b ? 1.0 : 0.0;
    case EX_LE: return a <= b ? 1.0 : 0.0;
    case EX_GT: return a > b ? 1.0 : 0.0;
    case EX_GE: return a >= b ? 1.0 : 0.0;
    case EX_EQ: return a == b ? 1.0 : 0.0;
    case EX_NE: return a != b ? 1.0 : 0.0;
  }
  return NA_REAL;
}

// Three-valued AND/OR on double-coded logicals (0 false, nonzero true, NA_REAL NA).
inline double bin_bool(int op, double a, double b) {
  bool ana = ISNAN(a), bna = ISNAN(b);
  if (op == EX_AND) {
    if ((!ana && a == 0.0) || (!bna && b == 0.0)) return 0.0;
    if (ana || bna) return NA_REAL;
    return 1.0;
  }
  // EX_OR
  if ((!ana && a != 0.0) || (!bna && b != 0.0)) return 1.0;
  if (ana || bna) return NA_REAL;
  return 0.0;
}

}  // namespace

extern "C" SEXP bt_expr_(SEXP df, SEXP s_code, SEXP s_args, SEXP s_consts) {
  Frame f = frame_from(df);
  R_xlen_t n = f.nrow;
  if (TYPEOF(s_code) != INTSXP || TYPEOF(s_args) != INTSXP || TYPEOF(s_consts) != REALSXP)
    Rf_error("basetable: malformed expression program");
  R_xlen_t ncode = Rf_xlength(s_code);
  if (Rf_xlength(s_args) != ncode)
    Rf_error("basetable: expression code/arg length mismatch");
  const int* code = INTEGER(s_code);
  const int* args = INTEGER(s_args);
  const double* consts = REAL(s_consts);
  R_xlen_t nconst = Rf_xlength(s_consts);

  std::vector<ExprVal> stack;
  stack.reserve(8);

  auto at = [n](const ExprVal& v, R_xlen_t i) -> double {
    return v.data.size() == 1 ? v.data[0] : v.data[(size_t)i];
  };

  for (R_xlen_t ip = 0; ip < ncode; ++ip) {
    int op = code[ip];
    switch (op) {
      case EX_COL: {
        int cj = args[ip];
        if (cj < 0 || cj >= f.ncol) Rf_error("basetable: expression column out of range");
        SEXP col = VECTOR_ELT(df, cj);
        ExprVal v;
        v.data.resize((size_t)n);
        for (R_xlen_t i = 0; i < n; ++i) v.data[(size_t)i] = col_as_double(col, i);
        v.logical = TYPEOF(col) == LGLSXP;
        stack.push_back(std::move(v));
        break;
      }
      case EX_CONST: {
        int ci = args[ip];
        if (ci < 0 || ci >= nconst) Rf_error("basetable: expression const out of range");
        ExprVal v; v.data.assign(1, consts[ci]); v.logical = false;
        stack.push_back(std::move(v));
        break;
      }
      case EX_TRUE:  { ExprVal v; v.data.assign(1, 1.0); v.logical = true; stack.push_back(std::move(v)); break; }
      case EX_FALSE: { ExprVal v; v.data.assign(1, 0.0); v.logical = true; stack.push_back(std::move(v)); break; }
      case EX_NA:    { ExprVal v; v.data.assign(1, NA_REAL); v.logical = true; stack.push_back(std::move(v)); break; }
      case EX_ADD: case EX_SUB: case EX_MUL: case EX_DIV: case EX_POW: case EX_MOD:
      case EX_LT: case EX_LE: case EX_GT: case EX_GE: case EX_EQ: case EX_NE:
      case EX_AND: case EX_OR: {
        if (stack.size() < 2) Rf_error("basetable: expression stack underflow");
        ExprVal b = std::move(stack.back()); stack.pop_back();
        ExprVal a = std::move(stack.back()); stack.pop_back();
        bool scalar = a.data.size() == 1 && b.data.size() == 1;
        ExprVal r;
        r.data.resize(scalar ? 1 : (size_t)n);
        bool cmp = (op >= EX_LT && op <= EX_NE);
        bool boolean = (op == EX_AND || op == EX_OR);
        r.logical = cmp || boolean;
        R_xlen_t m = scalar ? 1 : n;
        for (R_xlen_t i = 0; i < m; ++i) {
          double av = at(a, i), bv = at(b, i);
          r.data[(size_t)i] = boolean ? bin_bool(op, av, bv)
                            : cmp     ? bin_cmp(op, av, bv)
                                      : bin_arith(op, av, bv);
        }
        stack.push_back(std::move(r));
        break;
      }
      case EX_NEG: case EX_POS: case EX_NOT: {
        if (stack.empty()) Rf_error("basetable: expression stack underflow");
        ExprVal a = std::move(stack.back()); stack.pop_back();
        ExprVal r;
        r.data.resize(a.data.size());
        r.logical = (op == EX_NOT);
        for (size_t i = 0; i < a.data.size(); ++i) {
          double av = a.data[i];
          if (op == EX_NOT) r.data[i] = ISNAN(av) ? NA_REAL : (av == 0.0 ? 1.0 : 0.0);
          else if (op == EX_NEG) r.data[i] = ISNAN(av) ? NA_REAL : -av;
          else r.data[i] = av;
        }
        stack.push_back(std::move(r));
        break;
      }
      case EX_IFELSE: {
        if (stack.size() < 3) Rf_error("basetable: expression stack underflow");
        ExprVal no = std::move(stack.back()); stack.pop_back();
        ExprVal yes = std::move(stack.back()); stack.pop_back();
        ExprVal cond = std::move(stack.back()); stack.pop_back();
        bool scalar = cond.data.size() == 1 && yes.data.size() == 1 && no.data.size() == 1;
        ExprVal r;
        r.data.resize(scalar ? 1 : (size_t)n);
        r.logical = yes.logical && no.logical;
        R_xlen_t m = scalar ? 1 : n;
        for (R_xlen_t i = 0; i < m; ++i) {
          double c = at(cond, i);
          r.data[(size_t)i] = ISNAN(c) ? NA_REAL : (c != 0.0 ? at(yes, i) : at(no, i));
        }
        stack.push_back(std::move(r));
        break;
      }
      default:
        Rf_error("basetable: unknown expression opcode %d", op);
    }
  }

  if (stack.size() != 1) Rf_error("basetable: expression did not reduce to one value");
  ExprVal& top = stack.back();
  SEXP out;
  if (top.logical) {
    out = PROTECT(Rf_allocVector(LGLSXP, n));
    int* p = LOGICAL(out);
    for (R_xlen_t i = 0; i < n; ++i) {
      double v = at(top, i);
      p[i] = ISNAN(v) ? NA_LOGICAL : (v != 0.0 ? TRUE : FALSE);
    }
  } else {
    out = PROTECT(Rf_allocVector(REALSXP, n));
    double* p = REAL(out);
    for (R_xlen_t i = 0; i < n; ++i) p[i] = at(top, i);
  }
  UNPROTECT(1);
  return out;
}
