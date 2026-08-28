// R entry point for the writer. Columns are pre-resolved to plain C arrays on
// the R thread (including CHARSXP pointers), then a std::thread pool formats
// disjoint row ranges into private buffers that are flushed in order.

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "bt_common.h"

#include <cstdio>
#include <thread>
#include <string>
#include <vector>
#include <algorithm>

namespace {

struct ColView {
  int type;                       // INTSXP / REALSXP / LGLSXP / STRSXP
  const int* i = nullptr;
  const double* d = nullptr;
  std::vector<const char*> s;     // STRSXP: per-row C string (nullptr == NA)
};

inline void append_int(std::string& o, long long v) {
  char buf[24];
  int n = std::snprintf(buf, sizeof(buf), "%lld", v);
  o.append(buf, n);
}

inline void append_dbl(std::string& o, double v, int digits) {
  if (std::isnan(v)) { o.append("NaN"); return; }  // true NA handled by caller
  if (std::isinf(v)) { o.append(v < 0 ? "-Inf" : "Inf"); return; }
  char buf[40];
  int n = std::snprintf(buf, sizeof(buf), "%.*g", digits, v);
  o.append(buf, n);
}

inline void append_str(std::string& o, const char* p, char sep, char quote) {
  bool need = false;
  for (const char* q = p; *q; ++q) {
    if (*q == sep || *q == quote || *q == '\n' || *q == '\r') { need = true; break; }
  }
  if (!need) { o.append(p); return; }
  o.push_back(quote);
  for (const char* q = p; *q; ++q) {
    if (*q == quote) o.push_back(quote);
    o.push_back(*q);
  }
  o.push_back(quote);
}

void format_range(const std::vector<ColView>* cols, R_xlen_t r0, R_xlen_t r1,
                  char sep, char quote, int digits, const char* na,
                  std::string* out) {
  size_t na_len = std::strlen(na);
  std::string& o = *out;
  o.reserve((size_t)(r1 - r0) * cols->size() * 8);
  const size_t ncol = cols->size();
  for (R_xlen_t r = r0; r < r1; ++r) {
    for (size_t c = 0; c < ncol; ++c) {
      if (c) o.push_back(sep);
      const ColView& cv = (*cols)[c];
      switch (cv.type) {
        case INTSXP:
        case LGLSXP: {
          int v = cv.i[r];
          if (v == NA_INTEGER) o.append(na, na_len);
          else if (cv.type == LGLSXP) o.append(v ? "TRUE" : "FALSE");
          else append_int(o, v);
          break;
        }
        case REALSXP: {
          double v = cv.d[r];
          if (ISNA(v)) o.append(na, na_len);
          else append_dbl(o, v, digits);
          break;
        }
        case STRSXP: {
          const char* p = cv.s[r];
          if (!p) o.append(na, na_len);
          else append_str(o, p, sep, quote);
          break;
        }
      }
    }
    o.push_back('\n');
  }
}

} // namespace

extern "C" SEXP btwrite_(SEXP df, SEXP s_path, SEXP s_sep, SEXP s_quote,
                          SEXP s_na, SEXP s_digits, SEXP s_col_names,
                          SEXP s_append, SEXP s_threads) {
  const char* path = CHAR(STRING_ELT(s_path, 0));
  char sep   = CHAR(STRING_ELT(s_sep, 0))[0];
  char quote = CHAR(STRING_ELT(s_quote, 0))[0];
  const char* na = CHAR(STRING_ELT(s_na, 0));
  int digits = Rf_asInteger(s_digits);
  bool col_names = (Rf_asLogical(s_col_names) == TRUE);
  bool append = (Rf_asLogical(s_append) == TRUE);
  int n_threads = std::max(1, Rf_asInteger(s_threads));

  R_xlen_t ncol = Rf_xlength(df);
  if (ncol == 0) Rf_error("basetable: nothing to write (0 columns)");
  R_xlen_t nrow = Rf_xlength(VECTOR_ELT(df, 0));

  std::vector<ColView> cols((size_t) ncol);
  for (R_xlen_t c = 0; c < ncol; ++c) {
    SEXP col = VECTOR_ELT(df, c);
    ColView& cv = cols[(size_t) c];
    switch (TYPEOF(col)) {
      case INTSXP: {
        cv.type = INTSXP;
        if (Rf_inherits(col, "factor")) {
          // resolve factor codes to level strings up front
          SEXP lev = Rf_getAttrib(col, R_LevelsSymbol);
          cv.type = STRSXP;
          cv.s.resize((size_t) nrow);
          const int* codes = INTEGER(col);
          for (R_xlen_t r = 0; r < nrow; ++r)
            cv.s[(size_t) r] = (codes[r] == NA_INTEGER)
              ? nullptr : CHAR(STRING_ELT(lev, codes[r] - 1));
        } else {
          cv.i = INTEGER(col);
        }
        break;
      }
      case LGLSXP: cv.type = LGLSXP; cv.i = LOGICAL(col); break;
      case REALSXP: cv.type = REALSXP; cv.d = REAL(col); break;
      case STRSXP: {
        cv.type = STRSXP;
        cv.s.resize((size_t) nrow);
        for (R_xlen_t r = 0; r < nrow; ++r) {
          SEXP e = STRING_ELT(col, r);
          cv.s[(size_t) r] = (e == NA_STRING) ? nullptr : CHAR(e);
        }
        break;
      }
      default:
        Rf_error("basetable: column %lld has unsupported type '%s'",
                 (long long)(c + 1), Rf_type2char(TYPEOF(col)));
    }
  }

  FILE* f = std::fopen(path, append ? "ab" : "wb");
  if (!f) Rf_error("basetable: cannot open '%s' for writing", path);

  if (col_names && !append) {
    SEXP nm = Rf_getAttrib(df, R_NamesSymbol);
    std::string h;
    for (R_xlen_t c = 0; c < ncol; ++c) {
      if (c) h.push_back(sep);
      append_str(h, CHAR(STRING_ELT(nm, c)), sep, quote);
    }
    h.push_back('\n');
    std::fwrite(h.data(), 1, h.size(), f);
  }

  int nth = std::min<int>(n_threads, (int) std::max<R_xlen_t>(1, nrow / 8192));
  if (nth < 1) nth = 1;

  std::vector<std::string> chunks((size_t) nth);
  if (nth == 1) {
    format_range(&cols, 0, nrow, sep, quote, digits, na, &chunks[0]);
  } else {
    std::vector<std::thread> pool;
    R_xlen_t step = (nrow + nth - 1) / nth;
    for (int t = 0; t < nth; ++t) {
      R_xlen_t a = t * step, b = std::min<R_xlen_t>(nrow, a + step);
      if (a >= b) { chunks[(size_t) t].clear(); continue; }
      pool.emplace_back(format_range, &cols, a, b, sep, quote, digits, na,
                        &chunks[(size_t) t]);
    }
    for (auto& th : pool) th.join();
  }

  for (auto& ch : chunks)
    if (!ch.empty()) std::fwrite(ch.data(), 1, ch.size(), f);

  std::fclose(f);
  return s_path;
}
