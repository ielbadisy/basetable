// bt_aggregate(): fused delimited-file group-by.
//
// mmap the file, parallel newline index, then ONE pass per row: extract only
// the key and value fields, group, and accumulate. The key and value columns
// are never materialised as R vectors -- output is just the grouped result.
//
// Keys: integer / double / character, one or more columns (composite).
// Values: one or more numeric columns.
// Reducers: sum, mean, var, sd, min, max, n.

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "bt_common.h"

#include <thread>
#include <vector>
#include <string>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <algorithm>
#include <limits>

using namespace bt;

namespace {

enum KeyKind { K_INT = 0, K_DBL = 1, K_STR = 2 };
enum Fun { F_SUM, F_MEAN, F_VAR, F_SD, F_MIN, F_MAX, F_N };

Fun parse_fun(const char* s) {
  if (!strcmp(s, "sum"))  return F_SUM;
  if (!strcmp(s, "mean")) return F_MEAN;
  if (!strcmp(s, "var"))  return F_VAR;
  if (!strcmp(s, "sd"))   return F_SD;
  if (!strcmp(s, "min"))  return F_MIN;
  if (!strcmp(s, "max"))  return F_MAX;
  if (!strcmp(s, "n") || !strcmp(s, "count") || !strcmp(s, "length")) return F_N;
  Rf_error("basetable: unknown fun '%s'", s);
}

inline uint64_t fnv1a(const char* p, size_t n) {
  uint64_t h = 1469598103934665603ULL;
  for (size_t i = 0; i < n; ++i) { h ^= (unsigned char) p[i]; h *= 1099511628211ULL; }
  return h;
}

inline bool parse_i64(const char* p, size_t n, int64_t& out) {
  if (n == 0) return false;
  bool neg = false; size_t i = 0;
  if (p[0] == '-') { neg = true; i = 1; } else if (p[0] == '+') i = 1;
  if (i == n) return false;
  int64_t v = 0;
  for (; i < n; ++i) { char c = p[i]; if (c < '0' || c > '9') return false;
                       v = v * 10 + (c - '0'); }
  out = neg ? -v : v;
  return true;
}

// key blob schema, per key column, in order:
//   K_INT / K_DBL : 1 byte tag (0 = value, 1 = NA) + 8 bytes little-endian
//   K_STR         : 4 bytes len (0xFFFFFFFF = NA) + len bytes
struct Accum {
  std::vector<double> sum, sq, cnt, mn, mx;
  std::vector<char>   bad;      // ng*nv : group had NA value and !na_rm
};

struct Table {
  // open addressing: hslot -> group index; blobs stored contiguously
  std::vector<int64_t> hh;      // stored hash (0 = empty marker uses hused)
  std::vector<int>     hg;
  std::vector<char>    hused;
  size_t mask = 0;
  std::vector<std::string> blob;   // group -> key blob  (generic path)
  std::vector<int64_t>     nkey;   // group -> packed key (numeric fast path)
  bool numeric_key = false;
  Accum a;
  int nv = 0;
  bool need_sq = false, need_cnt = false, need_mn = false, need_mx = false;

  void init(size_t cap0, int nv_, bool sq, bool cnt, bool mn, bool mx, bool numkey) {
    size_t cap = 16; while (cap < cap0) cap <<= 1;
    hh.assign(cap, 0); hg.assign(cap, -1); hused.assign(cap, 0); mask = cap - 1;
    nv = nv_; need_sq = sq; need_cnt = cnt; need_mn = mn; need_mx = mx;
    numeric_key = numkey;
  }
  size_t n_groups() const { return numeric_key ? nkey.size() : blob.size(); }
  void grow_group() {
    a.sum.resize(a.sum.size() + nv, 0.0);
    if (need_sq)  a.sq.resize(a.sq.size() + nv, 0.0);
    if (need_cnt) a.cnt.resize(a.cnt.size() + nv, 0.0);
    if (need_mn)  a.mn.resize(a.mn.size() + nv,  std::numeric_limits<double>::infinity());
    if (need_mx)  a.mx.resize(a.mx.size() + nv, -std::numeric_limits<double>::infinity());
    a.bad.resize(a.bad.size() + nv, 0);
  }
  int find_or_add_num(int64_t k) {
    uint64_t hash = (uint64_t) k * 1099511628211ULL; hash ^= hash >> 29;
    size_t h = hash & mask;
    while (true) {
      if (!hused[h]) {
        int g = (int) nkey.size();
        hused[h] = 1; hh[h] = (int64_t) hash; hg[h] = g;
        nkey.push_back(k);
        grow_group();
        if ((nkey.size() * 2) > (mask + 1)) rehash();
        return g;
      }
      if ((uint64_t) hh[h] == hash && nkey[hg[h]] == k) return hg[h];
      h = (h + 1) & mask;
    }
  }
  void rehash() {
    size_t cap = (mask + 1) << 1, nm = cap - 1;
    std::vector<int64_t> nh(cap, 0); std::vector<int> ng(cap, -1); std::vector<char> nu(cap, 0);
    for (size_t i = 0; i <= mask; ++i) {
      if (!hused[i]) continue;
      size_t h = (uint64_t) hh[i] & nm;
      while (nu[h]) h = (h + 1) & nm;
      nh[h] = hh[i]; ng[h] = hg[i]; nu[h] = 1;
    }
    hh.swap(nh); hg.swap(ng); hused.swap(nu); mask = nm;
  }
  int find_or_add(uint64_t hash, const std::string& b) {
    size_t h = hash & mask;
    while (true) {
      if (!hused[h]) {
        int g = (int) blob.size();
        hused[h] = 1; hh[h] = (int64_t) hash; hg[h] = g;
        blob.push_back(b);
        grow_group();
        if ((blob.size() * 2) > (mask + 1)) { rehash(); }
        return g;
      }
      if ((uint64_t) hh[h] == hash && blob[hg[h]] == b) return hg[h];
      h = (h + 1) & mask;
    }
  }
};

struct Job {
  const char* data;
  const RowIndex* idx;
  const Options* opt;
  const std::vector<int>* by_col;      // 0-based field indices
  const std::vector<int>* by_kind;
  const std::vector<int>* val_col;     // 0-based field indices
  bool na_rm;
  bool need_sq, need_cnt, need_mn, need_mx;
  bool need_value = true;               // false when every fun is "n"

  // predicate pushdown: all AND-ed, evaluated in the same pass
  struct Pred { int col; int op; int kind; double dval; std::string sval; };
  const std::vector<Pred>* preds = nullptr;   // 0 <,1 <=,2 >,3 >=,4 ==,5 !=
};

static inline bool cmp_num(double a, int op, double b) {
  switch (op) { case 0: return a < b;  case 1: return a <= b;
                case 2: return a > b;  case 3: return a >= b;
                case 4: return a == b; case 5: return a != b; }
  return false;
}

void run_range(const Job& J, size_t r0, size_t r1, Table& T) {
  const int nk = (int) J.by_col->size();
  const int nv = (int) J.val_col->size();
  int maxfield = 0;
  for (int c : *J.by_col)  maxfield = std::max(maxfield, c);
  for (int c : *J.val_col) maxfield = std::max(maxfield, c);

  // fast path: a single integer or double key -> pack into one int64, no blob
  const bool numkey = (nk == 1 && (*J.by_kind)[0] != K_STR);
  const int  k0kind = nk ? (*J.by_kind)[0] : K_STR;
  const int  k0col  = nk ? (*J.by_col)[0]  : 0;
  T.init(1u << 12, nv, J.need_sq, J.need_cnt, J.need_mn, J.need_mx, numkey);

  const int npred = J.preds ? (int) J.preds->size() : 0;
  for (int i = 0; i < npred; ++i) maxfield = std::max(maxfield, (*J.preds)[i].col);

  std::string scratch, blob;
  std::vector<const char*> fb(maxfield + 1, nullptr);
  std::vector<size_t>      fn(maxfield + 1, 0);
  std::vector<char>        fok(maxfield + 1, 0);
  char nbuf[64];

  for (size_t r = r0; r < r1; ++r) {
    const char* row = J.data + J.idx->starts[r];
    size_t len = J.idx->starts[r + 1] - J.idx->starts[r];
    RowReader rr(row, len, *J.opt, &scratch, J.idx->any_quote);
    std::fill(fok.begin(), fok.end(), 0);
    for (int c = 0; c <= maxfield; ++c) {
      const char* p; size_t n;
      if (!rr.next(p, n)) break;
      fb[c] = p; fn[c] = n; fok[c] = 1;
    }

    // predicate pushdown: skip the row entirely if any AND-ed test fails
    bool pass = true;
    for (int i = 0; i < npred && pass; ++i) {
      const Job::Pred& pr = (*J.preds)[i];
      if (!fok[pr.col]) { pass = false; break; }
      const char* p = fb[pr.col]; size_t n = fn[pr.col];
      if (pr.kind == K_STR) {
        bool eq = (n == pr.sval.size() && std::memcmp(p, pr.sval.data(), n) == 0);
        pass = (pr.op == 4) ? eq : !eq;                  // == / !=
      } else {
        double v; bool okd = false;
        if (n < sizeof(nbuf)) { std::memcpy(nbuf, p, n); nbuf[n] = 0;
          char* e; v = std::strtod(nbuf, &e); okd = (e == nbuf + n); }
        pass = okd && cmp_num(v, pr.op, pr.dval);
      }
    }
    if (!pass) continue;

    int g;
    if (numkey) {
      bool have = fok[k0col];
      const char* p = have ? fb[k0col] : "";
      size_t n = have ? fn[k0col] : 0;
      bool na = !have || J.opt->is_na(p, n);
      int64_t packed;
      if (na) {
        packed = INT64_MIN;                       // NA sentinel
      } else if (k0kind == K_INT) {
        int64_t v; packed = parse_i64(p, n, v) ? v : INT64_MIN;
      } else {                                    // K_DBL: pack the bits
        double d = 0; bool okd = false;
        char buf[64];
        if (n < sizeof(buf)) { std::memcpy(buf, p, n); buf[n] = 0;
          char* e; d = std::strtod(buf, &e); okd = (e == buf + n); }
        if (!okd) packed = INT64_MIN;
        else { std::memcpy(&packed, &d, 8); if (packed == INT64_MIN) packed = INT64_MIN + 1; }
      }
      g = T.find_or_add_num(packed);
    } else {
      blob.clear();
      for (int j = 0; j < nk; ++j) {
        int c = (*J.by_col)[j];
        int kind = (*J.by_kind)[j];
        bool have = fok[c];
        const char* p = have ? fb[c] : "";
        size_t n = have ? fn[c] : 0;
        bool na = !have || J.opt->is_na(p, n);
        if (kind == K_STR) {
          uint32_t l = na ? 0xFFFFFFFFu : (uint32_t) n;
          blob.append((const char*) &l, 4);
          if (!na) blob.append(p, n);
        } else if (kind == K_INT) {
          char tag = 1; int64_t v = 0;
          if (!na && parse_i64(p, n, v)) tag = 0;
          blob.push_back(tag);
          blob.append((const char*) &v, 8);
        } else {
          char tag = 1; double d = 0;
          if (!na) {
            char buf[64];
            if (n < sizeof(buf)) { std::memcpy(buf, p, n); buf[n] = 0;
              char* e; d = std::strtod(buf, &e); if (e == buf + n) tag = 0; }
          }
          blob.push_back(tag);
          blob.append((const char*) &d, 8);
        }
      }
      g = T.find_or_add(fnv1a(blob.data(), blob.size()), blob);
    }

    // accumulate values
    if (!J.need_value) {
      for (int v = 0; v < nv; ++v)
        if (J.need_cnt) T.a.cnt[(size_t) g * nv + v] += 1.0;
      continue;
    }
    for (int v = 0; v < nv; ++v) {
      int c = (*J.val_col)[v];
      size_t off = (size_t) g * nv + v;
      bool have = fok[c];
      double x = 0; bool na = !have;
      if (have) {
        const char* p = fb[c]; size_t n = fn[c];
        if (J.opt->is_na(p, n)) na = true;
        else {
          char buf[64];
          if (n < sizeof(buf)) { std::memcpy(buf, p, n); buf[n] = 0;
            char* e; x = std::strtod(buf, &e); if (e != buf + n) na = true; }
          else na = true;
        }
      }
      if (na) { if (!J.na_rm) T.a.bad[off] = 1; continue; }
      if (J.need_mn && x < T.a.mn[off]) T.a.mn[off] = x;
      if (J.need_mx && x > T.a.mx[off]) T.a.mx[off] = x;
      T.a.sum[off] += x;
      if (J.need_sq)  T.a.sq[off]  += x * x;
      if (J.need_cnt) T.a.cnt[off] += 1.0;
    }
  }
}

} // namespace

extern "C" SEXP bt_agg_(SEXP s_path, SEXP s_delim, SEXP s_quote, SEXP s_comment,
                        SEXP s_header, SEXP s_skip, SEXP s_nmax, SEXP s_na,
                        SEXP s_threads, SEXP s_by_col, SEXP s_by_kind,
                        SEXP s_by_names, SEXP s_val_col, SEXP s_val_names,
                        SEXP s_funs, SEXP s_na_rm,
                        SEXP s_w_col, SEXP s_w_op, SEXP s_w_kind,
                        SEXP s_w_dval, SEXP s_w_sval) {
  std::string path = CHAR(STRING_ELT(s_path, 0));
  std::string err;
  auto file = MappedFile::open(path, err);
  if (!file->data && !err.empty()) Rf_error("basetable: %s ('%s')", err.c_str(), path.c_str());

  Options opt;
  { const char* d = CHAR(STRING_ELT(s_delim, 0));
    opt.delim = (strcmp(d, "\\t") == 0) ? '\t' : d[0]; }
  opt.quote   = CHAR(STRING_ELT(s_quote, 0))[0];
  opt.comment = CHAR(STRING_ELT(s_comment, 0))[0];
  opt.has_header = (Rf_asLogical(s_header) == TRUE);
  opt.skip  = (int64_t) Rf_asReal(s_skip);
  opt.n_max = Rf_isNull(s_nmax) ? -1 : (int64_t) Rf_asReal(s_nmax);
  opt.n_threads = std::max(1, Rf_asInteger(s_threads));
  opt.na_strings.clear();
  for (R_xlen_t i = 0; i < Rf_xlength(s_na); ++i)
    opt.na_strings.push_back(CHAR(STRING_ELT(s_na, i)));
  opt.refresh_na_fast_path();

  RowIndex index = build_row_index_mt(file->data, file->size, opt, opt.n_threads);
  int64_t nrow = (int64_t) index.starts.size() - 1;
  if (nrow < 0) nrow = 0;

  std::vector<int> by_col, by_kind, val_col;
  for (R_xlen_t i = 0; i < Rf_xlength(s_by_col);  ++i) by_col.push_back(INTEGER(s_by_col)[i] - 1);
  for (R_xlen_t i = 0; i < Rf_xlength(s_by_kind); ++i) by_kind.push_back(INTEGER(s_by_kind)[i]);
  for (R_xlen_t i = 0; i < Rf_xlength(s_val_col); ++i) val_col.push_back(INTEGER(s_val_col)[i] - 1);
  const int nk = (int) by_col.size();
  const int nv = (int) val_col.size();

  std::vector<Fun> funs;
  for (R_xlen_t i = 0; i < Rf_xlength(s_funs); ++i)
    funs.push_back(parse_fun(CHAR(STRING_ELT(s_funs, i))));
  const int nf = (int) funs.size();
  const bool na_rm = (Rf_asLogical(s_na_rm) == TRUE);

  bool need_sq = false, need_cnt = false, need_mn = false, need_mx = false;
  for (Fun f : funs) {
    if (f == F_VAR || f == F_SD) { need_sq = true; need_cnt = true; }
    if (f == F_MEAN || f == F_N) need_cnt = true;
    if (f == F_MIN) need_mn = true;
    if (f == F_MAX) need_mx = true;
  }

  bool need_value = false;
  for (Fun f : funs) if (f != F_N) need_value = true;

  std::vector<Job::Pred> preds;
  for (R_xlen_t i = 0; i < Rf_xlength(s_w_col); ++i) {
    Job::Pred pr;
    pr.col  = INTEGER(s_w_col)[i] - 1;
    pr.op   = INTEGER(s_w_op)[i];
    pr.kind = INTEGER(s_w_kind)[i];
    pr.dval = REAL(s_w_dval)[i];
    pr.sval = (STRING_ELT(s_w_sval, i) == NA_STRING) ? "" : CHAR(STRING_ELT(s_w_sval, i));
    preds.push_back(std::move(pr));
  }

  Job J{ file->data, &index, &opt, &by_col, &by_kind, &val_col, na_rm,
         need_sq, need_cnt, need_mn, need_mx, need_value, &preds };

  const bool numkey = (nk == 1 && by_kind[0] != K_STR);

  int nth = std::min<int>(opt.n_threads, (int) std::max<int64_t>(1, nrow / 65536));
  if (nth < 1) nth = 1;

  std::vector<Table> parts(nth);
  if (nth == 1) {
    run_range(J, 0, (size_t) nrow, parts[0]);
  } else {
    std::vector<std::thread> pool;
    int64_t chunk = (nrow + nth - 1) / nth;
    for (int t = 0; t < nth; ++t) {
      int64_t a = t * chunk, b = std::min<int64_t>(nrow, a + chunk);
      if (a >= b) { parts[t].init(16, nv, need_sq, need_cnt, need_mn, need_mx, numkey); break; }
      pool.emplace_back(run_range, std::cref(J), (size_t) a, (size_t) b, std::ref(parts[t]));
    }
    for (auto& th : pool) th.join();
  }

  // merge
  Table G;
  G.init(1u << 14, nv, need_sq, need_cnt, need_mn, need_mx, numkey);
  for (int t = 0; t < nth; ++t) {
    Table& L = parts[t];
    const size_t lng = L.n_groups();
    for (size_t gi = 0; gi < lng; ++gi) {
      int g = numkey ? G.find_or_add_num(L.nkey[gi])
                     : G.find_or_add(fnv1a(L.blob[gi].data(), L.blob[gi].size()), L.blob[gi]);
      for (int v = 0; v < nv; ++v) {
        size_t o = (size_t) g * nv + v, lo = gi * nv + v;
        G.a.sum[o] += L.a.sum[lo];
        if (need_sq)  G.a.sq[o]  += L.a.sq[lo];
        if (need_cnt) G.a.cnt[o] += L.a.cnt[lo];
        if (need_mn && L.a.mn[lo] < G.a.mn[o]) G.a.mn[o] = L.a.mn[lo];
        if (need_mx && L.a.mx[lo] > G.a.mx[o]) G.a.mx[o] = L.a.mx[lo];
        if (!na_rm && L.a.bad[lo]) G.a.bad[o] = 1;
      }
    }
  }
  const int ng = (int) G.n_groups();

  // ---- assemble output data.frame -----------------------------------
  int ncol_out = nk + nv * nf;
  SEXP out = PROTECT(Rf_allocVector(VECSXP, ncol_out));
  SEXP nm  = PROTECT(Rf_allocVector(STRSXP, ncol_out));

  // key columns
  std::vector<SEXP> keycol(nk);
  for (int j = 0; j < nk; ++j) {
    int kind = by_kind[j];
    SEXP col = Rf_allocVector(kind == K_INT ? INTSXP : kind == K_DBL ? REALSXP : STRSXP, ng);
    SET_VECTOR_ELT(out, j, col);
    keycol[j] = col;
    SET_STRING_ELT(nm, j, STRING_ELT(s_by_names, j));
  }
  // decode keys
  if (numkey) {
    for (int g = 0; g < ng; ++g) {
      int64_t k = G.nkey[g];
      if (by_kind[0] == K_INT) {
        INTEGER(keycol[0])[g] = (k == INT64_MIN || k > INT_MAX || k < INT_MIN + 1)
                                  ? NA_INTEGER : (int) k;
      } else {
        if (k == INT64_MIN) REAL(keycol[0])[g] = NA_REAL;
        else { double d; std::memcpy(&d, &k, 8); REAL(keycol[0])[g] = d; }
      }
    }
  } else
  for (int g = 0; g < ng; ++g) {
    const std::string& b = G.blob[g];
    size_t pos = 0;
    for (int j = 0; j < nk; ++j) {
      int kind = by_kind[j];
      if (kind == K_STR) {
        uint32_t l; std::memcpy(&l, b.data() + pos, 4); pos += 4;
        if (l == 0xFFFFFFFFu) SET_STRING_ELT(keycol[j], g, NA_STRING);
        else { SET_STRING_ELT(keycol[j], g, Rf_mkCharLenCE(b.data() + pos, (int) l, CE_UTF8));
               pos += l; }
      } else if (kind == K_INT) {
        char tag = b[pos]; int64_t v; std::memcpy(&v, b.data() + pos + 1, 8); pos += 9;
        INTEGER(keycol[j])[g] = (tag || v > INT_MAX || v < INT_MIN + 1) ? NA_INTEGER : (int) v;
      } else {
        char tag = b[pos]; double d; std::memcpy(&d, b.data() + pos + 1, 8); pos += 9;
        REAL(keycol[j])[g] = tag ? NA_REAL : d;
      }
    }
  }

  // value/fun columns
  int oc = nk;
  char buf[128];
  for (int v = 0; v < nv; ++v) {
    for (int fi = 0; fi < nf; ++fi, ++oc) {
      SEXP col = Rf_allocVector(REALSXP, ng);
      SET_VECTOR_ELT(out, oc, col);
      double* o = REAL(col);
      Fun f = funs[fi];
      for (int g = 0; g < ng; ++g) {
        size_t k = (size_t) g * nv + v;
        double S = G.a.sum[k];
        double C = need_cnt ? G.a.cnt[k] : 0;
        double Q = need_sq  ? G.a.sq[k]  : 0;
        double r;
        switch (f) {
          case F_SUM:  r = S; break;
          case F_N:    r = C; break;
          case F_MEAN: r = C > 0 ? S / C : NA_REAL; break;
          case F_MIN:  r = std::isinf(G.a.mn[k]) ? NA_REAL : G.a.mn[k]; break;
          case F_MAX:  r = std::isinf(G.a.mx[k]) ? NA_REAL : G.a.mx[k]; break;
          case F_VAR:  r = C > 1 ? (Q - S * S / C) / (C - 1) : NA_REAL; break;
          case F_SD:   r = C > 1 ? std::sqrt((Q - S * S / C) / (C - 1)) : NA_REAL; break;
          default:     r = NA_REAL;
        }
        o[g] = (!na_rm && G.a.bad[k]) ? NA_REAL : r;
      }
      const char* vn = CHAR(STRING_ELT(s_val_names, v));
      const char* fn_ = (f==F_SUM?"sum":f==F_MEAN?"mean":f==F_VAR?"var":f==F_SD?"sd":
                         f==F_MIN?"min":f==F_MAX?"max":"n");
      if (nf == 1 && nv >= 1) snprintf(buf, sizeof buf, "%s", vn);
      else snprintf(buf, sizeof buf, "%s_%s", vn, fn_);
      SET_STRING_ELT(nm, oc, Rf_mkCharCE(buf, CE_UTF8));
    }
  }

  Rf_setAttrib(out, R_NamesSymbol, nm);
  Rf_setAttrib(out, R_ClassSymbol, Rf_mkString("data.frame"));
  SEXP rn = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(rn)[0] = NA_INTEGER; INTEGER(rn)[1] = -ng;
  Rf_setAttrib(out, R_RowNamesSymbol, rn);

  UNPROTECT(3);
  return out;
}
