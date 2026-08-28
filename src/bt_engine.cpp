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
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace {

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
  Rf_setAttrib(out, R_ClassSymbol, Rf_mkString("data.frame"));
  UNPROTECT(2);
  return out;
}

void append_value_key(std::string& key, SEXP col, R_xlen_t i) {
  key.push_back((char)TYPEOF(col));
  key.push_back(':');
  switch (TYPEOF(col)) {
    case LGLSXP:
      key.append(std::to_string(LOGICAL(col)[i]));
      break;
    case INTSXP:
      key.append(std::to_string(INTEGER(col)[i]));
      break;
    case REALSXP: {
      double v = REAL(col)[i];
      if (ISNA(v)) key.append("NA");
      else if (std::isnan(v)) key.append("NaN");
      else {
        uint64_t bits;
        std::memcpy(&bits, &v, sizeof(double));
        key.append(std::to_string(bits));
      }
      break;
    }
    case STRSXP: {
      SEXP s = STRING_ELT(col, i);
      if (s == NA_STRING) key.append("<NA>");
      else {
        const char* p = CHAR(s);
        key.append(p, std::strlen(p));
      }
      break;
    }
    default:
      Rf_error("basetable: unsupported key column type '%s'", Rf_type2char(TYPEOF(col)));
  }
  key.push_back('\037');
}

std::string row_key(SEXP df, const std::vector<int>& cols, R_xlen_t i) {
  std::string key;
  key.reserve(cols.size() * 16);
  for (int j : cols) append_value_key(key, VECTOR_ELT(df, j), i);
  return key;
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
  Rf_setAttrib(out, R_ClassSymbol, Rf_mkString("data.frame"));
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
  Rf_setAttrib(out, R_ClassSymbol, Rf_mkString("data.frame"));
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
  std::vector<R_xlen_t> ord;
  ord.reserve((size_t)f.nrow);
  for (R_xlen_t i = 0; i < f.nrow; ++i) ord.push_back(i);
  std::stable_sort(ord.begin(), ord.end(), [&](R_xlen_t a, R_xlen_t b) {
    for (R_xlen_t k = 0; k < (R_xlen_t)by.size(); ++k) {
      int c = cmp_value(VECTOR_ELT(df, by[(size_t)k]), a, b, na_last);
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
  if (by.size() != 1 || !unique_single(VECTOR_ELT(df, by[0]), f.nrow, rows)) {
    std::unordered_set<std::string> seen;
    seen.reserve((size_t)f.nrow);
    rows.reserve((size_t)f.nrow);
    for (R_xlen_t i = 0; i < f.nrow; ++i) {
      std::string key = row_key(df, by, i);
      if (seen.emplace(std::move(key)).second) rows.push_back(i);
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
    std::unordered_set<std::string> seen;
    seen.reserve((size_t)f.nrow);
    if (from_last) {
      for (R_xlen_t i = f.nrow; i-- > 0;) {
        std::string key = row_key(df, by, i);
        p[i] = seen.emplace(std::move(key)).second ? FALSE : TRUE;
      }
    } else {
      for (R_xlen_t i = 0; i < f.nrow; ++i) {
        std::string key = row_key(df, by, i);
        p[i] = seen.emplace(std::move(key)).second ? FALSE : TRUE;
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
  std::unordered_map<std::string, int> pos;
  pos.reserve((size_t)f.nrow);
  std::vector<R_xlen_t> first;
  std::vector<int> counts;
  for (R_xlen_t i = 0; i < f.nrow; ++i) {
    std::string key = row_key(df, by, i);
    auto it = pos.find(key);
    if (it == pos.end()) {
      int p = (int)first.size();
      pos.emplace(std::move(key), p);
      first.push_back(i);
      counts.push_back(1);
    } else {
      counts[(size_t)it->second]++;
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
  Rf_setAttrib(with_n, R_ClassSymbol, Rf_mkString("data.frame"));
  UNPROTECT(4);
  return with_n;
}
