// Lazy numeric columns: an ALTREP wrapper that parses its column the first
// time R asks for the data pointer. data1 = list(xptr_to_LazySource, col);
// data2 = materialised vector cache (nil until first touch).

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Altrep.h>

#include "bt_common.h"

using namespace bt;

static R_altrep_class_t bt_real_class;
static R_altrep_class_t bt_int_class;
static R_altrep_class_t bt_str_class;

// ---------------------------------------------------------------------------

extern "C" void bt_lazysource_finalizer(SEXP xptr) {
  LazySource* s = static_cast<LazySource*>(R_ExternalPtrAddr(xptr));
  if (s) { delete s; R_ClearExternalPtr(xptr); }
}

static LazySource* alt_src(SEXP x) {
  SEXP d1 = R_altrep_data1(x);
  return static_cast<LazySource*>(R_ExternalPtrAddr(VECTOR_ELT(d1, 0)));
}
static int alt_col(SEXP x) {
  return INTEGER(VECTOR_ELT(R_altrep_data1(x), 1))[0];
}

static SEXP alt_materialise(SEXP x, int type) {
  SEXP cache = R_altrep_data2(x);
  if (cache != R_NilValue) return cache;
  LazySource* s = alt_src(x);
  int col = alt_col(x);
  SEXP v = PROTECT(Rf_allocVector(type == COL_INTEGER ? INTSXP : REALSXP, s->nrow));
  int fail = 0;
  void* ptr = (type == COL_INTEGER) ? (void*) INTEGER(v) : (void*) REAL(v);
  materialise_column(*s, col, type, ptr, &fail);
  if (fail > 0)
    Rf_warning("basetable: %d value(s) in lazy column '%s' did not parse as %s; set to NA",
               fail, s->names[col].c_str(), type == COL_INTEGER ? "integer" : "double");
  R_set_altrep_data2(x, v);
  UNPROTECT(1);
  return v;
}

// ---- shared ALTREP methods ------------------------------------------------

static R_xlen_t bt_Length(SEXP x) {
  return (R_xlen_t) alt_src(x)->nrow;
}

static Rboolean bt_Inspect(SEXP x, int, int, int, void (*)(SEXP, int, int, int)) {
  LazySource* s = alt_src(x);
  Rprintf(" basetable lazy column '%s' (n=%lld, %s)\n",
          s->names[alt_col(x)].c_str(), (long long) s->nrow,
          R_altrep_data2(x) == R_NilValue ? "unrealised" : "realised");
  return TRUE;
}

static const void* bt_real_Dataptr_or_null(SEXP x) {
  SEXP c = R_altrep_data2(x);
  return c == R_NilValue ? nullptr : (const void*) REAL(c);
}
static void* bt_real_Dataptr(SEXP x, Rboolean) {
  return REAL(alt_materialise(x, COL_DOUBLE));
}
static double bt_real_Elt(SEXP x, R_xlen_t i) {
  return REAL(alt_materialise(x, COL_DOUBLE))[i];
}
static R_xlen_t bt_real_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, double* buf) {
  SEXP v = alt_materialise(x, COL_DOUBLE);
  R_xlen_t ncopy = (i + n > XLENGTH(v)) ? XLENGTH(v) - i : n;
  if (ncopy > 0) std::memcpy(buf, REAL(v) + i, ncopy * sizeof(double));
  return ncopy < 0 ? 0 : ncopy;
}

static const void* bt_int_Dataptr_or_null(SEXP x) {
  SEXP c = R_altrep_data2(x);
  return c == R_NilValue ? nullptr : (const void*) INTEGER(c);
}
static void* bt_int_Dataptr(SEXP x, Rboolean) {
  return INTEGER(alt_materialise(x, COL_INTEGER));
}
static int bt_int_Elt(SEXP x, R_xlen_t i) {
  return INTEGER(alt_materialise(x, COL_INTEGER))[i];
}
static R_xlen_t bt_int_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, int* buf) {
  SEXP v = alt_materialise(x, COL_INTEGER);
  R_xlen_t ncopy = (i + n > XLENGTH(v)) ? XLENGTH(v) - i : n;
  if (ncopy > 0) std::memcpy(buf, INTEGER(v) + i, ncopy * sizeof(int));
  return ncopy < 0 ? 0 : ncopy;
}

// ---- string column -----------------------------------------------------

static SEXP bt_str_materialise(SEXP x) {
  SEXP cache = R_altrep_data2(x);
  if (cache != R_NilValue) return cache;
  LazySource* s = alt_src(x);
  int col = alt_col(x);
  SEXP v = PROTECT(Rf_allocVector(STRSXP, s->nrow));
  materialise_string_column(*s, col, v);
  R_set_altrep_data2(x, v);
  UNPROTECT(1);
  return v;
}
static SEXP bt_str_Elt(SEXP x, R_xlen_t i) {
  return STRING_ELT(bt_str_materialise(x), i);
}
static void bt_str_Set_elt(SEXP x, R_xlen_t i, SEXP v) {
  SET_STRING_ELT(bt_str_materialise(x), i, v);
}

// ---- registration + constructors ---------------------------------------

extern "C" void bt_init_altrep(DllInfo* dll) {
  bt_real_class = R_make_altreal_class("bt_lazy_real", "basetable", dll);
  R_set_altrep_Length_method(bt_real_class, bt_Length);
  R_set_altrep_Inspect_method(bt_real_class, bt_Inspect);
  R_set_altvec_Dataptr_method(bt_real_class, bt_real_Dataptr);
  R_set_altvec_Dataptr_or_null_method(bt_real_class, bt_real_Dataptr_or_null);
  R_set_altreal_Elt_method(bt_real_class, bt_real_Elt);
  R_set_altreal_Get_region_method(bt_real_class, bt_real_Get_region);

  bt_int_class = R_make_altinteger_class("bt_lazy_int", "basetable", dll);
  R_set_altrep_Length_method(bt_int_class, bt_Length);
  R_set_altrep_Inspect_method(bt_int_class, bt_Inspect);
  R_set_altvec_Dataptr_method(bt_int_class, bt_int_Dataptr);
  R_set_altvec_Dataptr_or_null_method(bt_int_class, bt_int_Dataptr_or_null);
  R_set_altinteger_Elt_method(bt_int_class, bt_int_Elt);
  R_set_altinteger_Get_region_method(bt_int_class, bt_int_Get_region);

  bt_str_class = R_make_altstring_class("bt_lazy_str", "basetable", dll);
  R_set_altrep_Length_method(bt_str_class, bt_Length);
  R_set_altrep_Inspect_method(bt_str_class, bt_Inspect);
  R_set_altstring_Elt_method(bt_str_class, bt_str_Elt);
  R_set_altstring_Set_elt_method(bt_str_class, bt_str_Set_elt);
}

static SEXP bt_make_altrep(SEXP xptr, int col, bool is_int) {
  SEXP d1 = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(d1, 0, xptr);
  SET_VECTOR_ELT(d1, 1, Rf_ScalarInteger(col));
  SEXP res = R_new_altrep(is_int ? bt_int_class : bt_real_class, d1, R_NilValue);
  UNPROTECT(1);
  return res;
}

extern "C" SEXP bt_make_altrep_real(SEXP xptr, int col) {
  return bt_make_altrep(xptr, col, false);
}
extern "C" SEXP bt_make_altrep_int(SEXP xptr, int col) {
  return bt_make_altrep(xptr, col, true);
}
extern "C" SEXP bt_make_altrep_str(SEXP xptr, int col) {
  SEXP d1 = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(d1, 0, xptr);
  SET_VECTOR_ELT(d1, 1, Rf_ScalarInteger(col));
  SEXP res = R_new_altrep(bt_str_class, d1, R_NilValue);
  UNPROTECT(1);
  return res;
}
