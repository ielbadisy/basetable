// R entry point for the reader: sniff, type-guess, materialise.
//
// Eager path  : numeric/logical columns filled by a std::thread pool,
//               character columns built on the R thread (mkCharLenCE).
// Lazy path   : numeric columns handed back as ALTREP (see bt_altrep.cpp);
//               only touched columns are ever parsed.

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "bt_common.h"

#include <thread>
#include <algorithm>
#include <cmath>

using namespace bt;

extern "C" {
  void bt_lazysource_finalizer(SEXP);
  SEXP bt_make_altrep_real(SEXP, int);
  SEXP bt_make_altrep_int(SEXP, int);
}

// ---------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------

static char sniff_delim(const char* p, const char* end) {
  const char cands[] = { ',', '\t', ';', '|', ' ' };
  size_t best_count = 0;
  char best = ',';
  const char* nl = static_cast<const char*>(memchr(p, '\n', end - p));
  const char* stop = nl ? nl : end;
  for (char c : cands) {
    size_t n = 0;
    bool inq = false;
    for (const char* q = p; q < stop; ++q) {
      if (*q == '"') inq = !inq;
      else if (*q == c && !inq) ++n;
    }
    if (n > best_count) { best_count = n; best = c; }
  }
  return best;
}

static int narrowest_type(const char* p, size_t n) {
  int b; double d;
  if (parse_bool(p, n, b)) return COL_LOGICAL;
  if (parse_int32(p, n, b)) return COL_INTEGER;
  if (parse_double(p, n, d)) return COL_DOUBLE;
  return COL_STRING;
}

// fill a contiguous row range [r0, r1) for every numeric/logical kept column
static void convert_numeric_range(const LazySource& src,
                                  const std::vector<int>& out_col,   // full -> kept slot, -1 if dropped
                                  const std::vector<void*>& out_ptr, // per kept slot
                                  size_t r0, size_t r1,
                                  std::vector<std::atomic<int>>& fail) {
  const Options& opt = src.opt;
  const char* data = src.file->data;
  const int ncol = src.ncol;
  std::string scratch;

  // last kept numeric column: nothing to do past it on each row
  int last_active = -1;
  for (int c = 0; c < ncol; ++c)
    if (out_col[c] >= 0 && src.types[c] != COL_STRING && out_ptr[out_col[c]]) last_active = c;
  if (last_active < 0) return;

  for (size_t r = r0; r < r1; ++r) {
    const char* row = data + src.index.starts[r];
    size_t len = src.index.starts[r + 1] - src.index.starts[r];
    RowReader rr(row, len, opt, &scratch, src.index.any_quote);

    for (int c = 0; c <= last_active; ++c) {
      const char* fp = nullptr; size_t fn = 0;
      bool have = rr.next(fp, fn);

      int slot = out_col[c];
      if (slot < 0) continue;
      int type = src.types[c];
      if (type == COL_STRING) continue;
      if (out_ptr[slot] == nullptr) continue;  // lazy (ALTREP) column: skip

      bool na = !have || fn == 0 || opt.is_na(fp, fn);

      if (type == COL_LOGICAL) {
        int* o = static_cast<int*>(out_ptr[slot]);
        int b;
        if (na) o[r] = NA_LOGICAL;
        else if (parse_bool(fp, fn, b)) o[r] = b;
        else { o[r] = NA_LOGICAL; fail[slot].fetch_add(1, std::memory_order_relaxed); }
      } else if (type == COL_INTEGER) {
        int* o = static_cast<int*>(out_ptr[slot]);
        int v;
        if (na) o[r] = NA_INTEGER;
        else if (parse_int32(fp, fn, v)) o[r] = v;
        else { o[r] = NA_INTEGER; fail[slot].fetch_add(1, std::memory_order_relaxed); }
      } else { // COL_DOUBLE
        double* o = static_cast<double*>(out_ptr[slot]);
        double v;
        if (na) o[r] = NA_REAL;
        else if (parse_double(fp, fn, v)) o[r] = v;
        else { o[r] = NA_REAL; fail[slot].fetch_add(1, std::memory_order_relaxed); }
      }
    }
  }
}

// single-column materialiser used by the ALTREP path
void bt::materialise_column(const LazySource& src, int col, int type,
                            void* out_ptr, int* n_parse_fail) {
  std::vector<int> out_col(src.ncol, -1);
  out_col[col] = 0;
  std::vector<void*> out_ptr_v(1, out_ptr);
  std::vector<std::atomic<int>> fail(1);
  fail[0].store(0);
  convert_numeric_range(src, out_col, out_ptr_v, 0, (size_t) src.nrow, fail);
  if (n_parse_fail) *n_parse_fail = fail[0].load();
}

// ---------------------------------------------------------------------------
// .Call entry
// ---------------------------------------------------------------------------

extern "C" SEXP btread_(SEXP s_path, SEXP s_delim, SEXP s_quote, SEXP s_comment,
                         SEXP s_header, SEXP s_trim, SEXP s_skip, SEXP s_nmax,
                         SEXP s_na, SEXP s_threads, SEXP s_guess_max,
                         SEXP s_col_names, SEXP s_col_types, SEXP s_col_select,
                         SEXP s_lazy) {
  std::string path = CHAR(STRING_ELT(s_path, 0));
  std::string err;
  auto file = MappedFile::open(path, err);
  if (!file->data && !err.empty()) Rf_error("basetable: %s ('%s')", err.c_str(), path.c_str());

  Options opt;
  bool sniff = false;
  {
    const char* d = CHAR(STRING_ELT(s_delim, 0));
    if (d[0] == '\0') { sniff = true; opt.delim = ','; } // resolved after indexing
    else if (strcmp(d, "\\t") == 0) opt.delim = '\t';
    else opt.delim = d[0];
  }
  opt.quote   = CHAR(STRING_ELT(s_quote, 0))[0];
  { const char* c = CHAR(STRING_ELT(s_comment, 0)); opt.comment = c[0]; }
  opt.has_header = (Rf_asLogical(s_header) == TRUE);
  opt.trim_ws   = (Rf_asLogical(s_trim) == TRUE);
  opt.skip      = (int64_t) Rf_asReal(s_skip);
  opt.n_max     = Rf_isNull(s_nmax) ? -1 : (int64_t) Rf_asReal(s_nmax);
  opt.n_threads = std::max(1, Rf_asInteger(s_threads));
  {
    opt.na_strings.clear();
    for (R_xlen_t i = 0; i < Rf_xlength(s_na); ++i)
      opt.na_strings.push_back(CHAR(STRING_ELT(s_na, i)));
  }

  RowIndex index = build_row_index_mt(file->data, file->size, opt, opt.n_threads);
  int64_t nrow = (int64_t) index.starts.size() - 1;
  if (nrow < 0) nrow = 0;

  // delimiter sniffing, now that we know where the first real line begins
  if (sniff) {
    size_t off = (opt.has_header && index.has_header_line)
                   ? index.header_off
                   : (nrow > 0 ? index.starts[0] : 0);
    opt.delim = sniff_delim(file->data + off, file->data + file->size);
  }

  // ---- column names + count -------------------------------------------------
  std::vector<std::string> names;
  std::vector<std::pair<const char*, size_t>> fields;
  std::string scratch;

  if (opt.has_header && index.has_header_line) {
    size_t hstart = index.header_off;
    size_t hend = (nrow > 0) ? index.starts[0] : file->size;
    split_row(file->data + hstart, hend - hstart, opt, fields, scratch);
    for (auto& f : fields) names.emplace_back(f.first, f.second);
  } else {
    size_t nf = 0;
    if (nrow > 0) {
      nf = split_row(file->data + index.starts[0],
                     index.starts[1] - index.starts[0], opt, fields, scratch);
    }
    if (!Rf_isNull(s_col_names) && (size_t) Rf_xlength(s_col_names) == nf) {
      for (R_xlen_t i = 0; i < Rf_xlength(s_col_names); ++i)
        names.push_back(CHAR(STRING_ELT(s_col_names, i)));
    } else {
      for (size_t i = 0; i < nf; ++i) names.push_back("V" + std::to_string(i + 1));
    }
  }
  int ncol = (int) names.size();
  if (ncol == 0) Rf_error("basetable: could not determine any columns");

  // ---- type guessing -----------------------------------------------------
  std::vector<int> types(ncol, COL_LOGICAL);
  std::vector<char> seen_value(ncol, 0);
  int64_t guess_max = (int64_t) Rf_asReal(s_guess_max);
  if (guess_max <= 0) guess_max = 10000;
  int64_t nsample = std::min<int64_t>(nrow, guess_max);
  double stride = (nsample > 0) ? (double) nrow / (double) nsample : 1.0;

  for (int64_t s = 0; s < nsample; ++s) {
    int64_t r = (int64_t) (s * stride);
    if (r >= nrow) r = nrow - 1;
    const char* row = file->data + index.starts[r];
    size_t len = index.starts[r + 1] - index.starts[r];
    size_t nf = split_row(row, len, opt, fields, scratch);
    for (int c = 0; c < ncol && (size_t) c < nf; ++c) {
      const char* fp = fields[c].first; size_t fn = fields[c].second;
      if (opt.is_na(fp, fn)) continue;
      seen_value[c] = 1;
      int t = narrowest_type(fp, fn);
      if (t > types[c]) types[c] = t;
    }
  }
  for (int c = 0; c < ncol; ++c)
    if (!seen_value[c]) types[c] = COL_LOGICAL; // all-NA column

  // ---- explicit col_types override -------------------------------------
  if (!Rf_isNull(s_col_types) && Rf_xlength(s_col_types) == ncol) {
    for (int c = 0; c < ncol; ++c) {
      const char* t = CHAR(STRING_ELT(s_col_types, c));
      if      (strcmp(t, "logical")   == 0) types[c] = COL_LOGICAL;
      else if (strcmp(t, "integer")   == 0) types[c] = COL_INTEGER;
      else if (strcmp(t, "double")    == 0) types[c] = COL_DOUBLE;
      else if (strcmp(t, "character") == 0) types[c] = COL_STRING;
      else if (strcmp(t, "skip")      == 0) types[c] = COL_SKIP;
      // "guess" leaves the inferred type in place
    }
  }

  // ---- col_select -----------------------------------------------------
  std::vector<int> out_col(ncol, -1);
  int nkeep = 0;
  if (!Rf_isNull(s_col_select) && Rf_xlength(s_col_select) > 0) {
    std::vector<char> want(ncol, 0);
    for (R_xlen_t i = 0; i < Rf_xlength(s_col_select); ++i) {
      int k = INTEGER(s_col_select)[i] - 1;
      if (k >= 0 && k < ncol) want[k] = 1;
    }
    for (int c = 0; c < ncol; ++c)
      if (want[c] && types[c] != COL_SKIP) out_col[c] = nkeep++;
  } else {
    for (int c = 0; c < ncol; ++c)
      if (types[c] != COL_SKIP) out_col[c] = nkeep++;
  }
  if (nkeep == 0) Rf_error("basetable: no columns selected");

  bool lazy = (Rf_asLogical(s_lazy) == TRUE);

  // shared source object (also owns the mapping for the lazy path)
  LazySource* src = new LazySource();
  src->file  = std::move(file);
  src->index = std::move(index);
  src->opt   = opt;
  src->ncol  = ncol;
  src->types = types;
  src->names = names;
  src->nrow  = nrow;

  const char* data = src->file->data; // after move

  // ---- allocate output list ------------------------------------------
  SEXP out = PROTECT(Rf_allocVector(VECSXP, nkeep));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, nkeep));

  std::vector<void*> num_ptr(nkeep, nullptr);         // numeric/logical slots
  std::vector<int>   slot_type(nkeep, COL_STRING);
  std::vector<int>   slot_fullcol(nkeep, -1);

  SEXP src_xptr = R_NilValue;
  if (lazy) {
    // one external pointer, shared by every ALTREP column; frees src on GC
    src_xptr = PROTECT(R_MakeExternalPtr(src, Rf_install("bt_lazysource"), R_NilValue));
    R_RegisterCFinalizerEx(src_xptr, bt_lazysource_finalizer, TRUE);
  }

  for (int c = 0; c < ncol; ++c) {
    int slot = out_col[c];
    if (slot < 0) continue;
    SET_STRING_ELT(out_names, slot, Rf_mkCharLenCE(names[c].data(), names[c].size(), CE_UTF8));
    slot_fullcol[slot] = c;
    int type = types[c];
    slot_type[slot] = type;

    if (lazy && (type == COL_INTEGER || type == COL_DOUBLE)) {
      SEXP col = (type == COL_INTEGER) ? bt_make_altrep_int(src_xptr, c)
                                       : bt_make_altrep_real(src_xptr, c);
      SET_VECTOR_ELT(out, slot, col);
    } else if (type == COL_LOGICAL) {
      SEXP v = Rf_allocVector(LGLSXP, nrow);
      SET_VECTOR_ELT(out, slot, v);
      num_ptr[slot] = LOGICAL(v);
    } else if (type == COL_INTEGER) {
      SEXP v = Rf_allocVector(INTSXP, nrow);
      SET_VECTOR_ELT(out, slot, v);
      num_ptr[slot] = INTEGER(v);
    } else if (type == COL_DOUBLE) {
      SEXP v = Rf_allocVector(REALSXP, nrow);
      SET_VECTOR_ELT(out, slot, v);
      num_ptr[slot] = REAL(v);
    } else { // COL_STRING
      SEXP v = Rf_allocVector(STRSXP, nrow);
      SET_VECTOR_ELT(out, slot, v);
    }
  }

  // ---- eager numeric fill (threaded) --------------------------------
  bool any_eager_numeric = false;
  for (int slot = 0; slot < nkeep; ++slot)
    if (num_ptr[slot]) any_eager_numeric = true;

  std::vector<std::atomic<int>> fail(nkeep);
  for (int i = 0; i < nkeep; ++i) fail[i].store(0);

  if (any_eager_numeric && nrow > 0) {
    int nth = std::min<int>(opt.n_threads, (int) std::max<int64_t>(1, nrow / 4096));
    if (nth < 1) nth = 1;
    if (nth == 1) {
      convert_numeric_range(*src, out_col, num_ptr, 0, (size_t) nrow, fail);
    } else {
      std::vector<std::thread> pool;
      int64_t chunk = (nrow + nth - 1) / nth;
      for (int t = 0; t < nth; ++t) {
        int64_t a = t * chunk, b = std::min<int64_t>(nrow, a + chunk);
        if (a >= b) break;
        pool.emplace_back(convert_numeric_range, std::cref(*src), std::cref(out_col),
                          std::cref(num_ptr), (size_t) a, (size_t) b, std::ref(fail));
      }
      for (auto& th : pool) th.join();
    }
  }

  // ---- eager character fill (R thread; also runs lazy string cols) --
  bool any_string = false;
  for (int slot = 0; slot < nkeep; ++slot)
    if (slot_type[slot] == COL_STRING) { any_string = true; break; }

  if (any_string && nrow > 0) {
    // full-column index -> STRSXP slot (or -1); iterate only up to the last one
    std::vector<int> str_slot(ncol, -1);
    int last_str = -1;
    for (int slot = 0; slot < nkeep; ++slot)
      if (slot_type[slot] == COL_STRING) {
        str_slot[slot_fullcol[slot]] = slot;
        if (slot_fullcol[slot] > last_str) last_str = slot_fullcol[slot];
      }

    std::string sc2;
    for (int64_t r = 0; r < nrow; ++r) {
      const char* row = data + src->index.starts[r];
      size_t len = src->index.starts[r + 1] - src->index.starts[r];
      RowReader rr(row, len, opt, &sc2, src->index.any_quote);
      for (int c = 0; c <= last_str; ++c) {
        const char* fp = nullptr; size_t fn = 0;
        bool have = rr.next(fp, fn);
        int slot = str_slot[c];
        if (slot < 0) continue;
        SEXP v = VECTOR_ELT(out, slot);
        if (!have || opt.is_na(fp, fn)) SET_STRING_ELT(v, r, NA_STRING);
        else SET_STRING_ELT(v, r, Rf_mkCharLenCE(fp, (int) fn, CE_UTF8));
      }
    }
  }

  // ---- parse-failure warnings -------------------------------------
  for (int slot = 0; slot < nkeep; ++slot) {
    int nf = fail[slot].load();
    if (nf > 0)
      Rf_warning("basetable: %d value(s) in column '%s' did not parse as %s; set to NA",
                 nf, names[slot_fullcol[slot]].c_str(),
                 slot_type[slot] == COL_INTEGER ? "integer" :
                 slot_type[slot] == COL_DOUBLE  ? "double"  : "logical");
  }

  // ---- data.frame wrapper --------------------------------------------
  Rf_setAttrib(out, R_NamesSymbol, out_names);
  SEXP cls = PROTECT(Rf_mkString("data.frame"));
  Rf_setAttrib(out, R_ClassSymbol, cls);
  if (nrow <= 2147483647LL) {
    SEXP rn = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(rn)[0] = NA_INTEGER;
    INTEGER(rn)[1] = -(int) nrow;
    Rf_setAttrib(out, R_RowNamesSymbol, rn);
    UNPROTECT(1);
  } else {
    SEXP rn = PROTECT(Rf_allocVector(REALSXP, nrow));
    for (int64_t i = 0; i < nrow; ++i) REAL(rn)[i] = (double) (i + 1);
    Rf_setAttrib(out, R_RowNamesSymbol, rn);
    UNPROTECT(1);
  }

  if (!lazy) {
    delete src;                 // eager path: mapping no longer needed
    UNPROTECT(3);               // out, out_names, cls
  } else {
    UNPROTECT(4);               // out, out_names, src_xptr, cls
  }
  return out;
}
