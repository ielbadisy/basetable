// Routine registration for the compiled part of basetable.

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

extern "C" {

SEXP btread_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
              SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP btwrite_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP bt_agg_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
             SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP bt_subset_(SEXP, SEXP, SEXP, SEXP);
SEXP bt_order_(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP bt_unique_(SEXP, SEXP, SEXP);
SEXP bt_duplicated_(SEXP, SEXP, SEXP);
SEXP bt_count_(SEXP, SEXP, SEXP);
SEXP bt_group_agg_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP bt_match_mask_(SEXP, SEXP, SEXP, SEXP);
SEXP bt_group_id_(SEXP, SEXP);
SEXP bt_join_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP bt_first_match_(SEXP, SEXP, SEXP, SEXP);
SEXP bt_range_join_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP bt_rolling_join_(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP bt_expr_(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP bt_rbind_(SEXP, SEXP, SEXP, SEXP);
void bt_init_altrep(DllInfo* dll);

static const R_CallMethodDef CallEntries[] = {
  { "btread_",  (DL_FUNC) &btread_,  15 },
  { "btwrite_", (DL_FUNC) &btwrite_,  9 },
  { "bt_agg_",  (DL_FUNC) &bt_agg_,  21 },
  { "bt_subset_",     (DL_FUNC) &bt_subset_,     4 },
  { "bt_order_",      (DL_FUNC) &bt_order_,      5 },
  { "bt_unique_",     (DL_FUNC) &bt_unique_,     3 },
  { "bt_duplicated_", (DL_FUNC) &bt_duplicated_, 3 },
  { "bt_count_",      (DL_FUNC) &bt_count_,      3 },
  { "bt_group_agg_",  (DL_FUNC) &bt_group_agg_,  6 },
  { "bt_match_mask_", (DL_FUNC) &bt_match_mask_, 4 },
  { "bt_group_id_",   (DL_FUNC) &bt_group_id_,   2 },
  { "bt_join_",         (DL_FUNC) &bt_join_,         7 },
  { "bt_first_match_",  (DL_FUNC) &bt_first_match_,  4 },
  { "bt_range_join_",   (DL_FUNC) &bt_range_join_,   9 },
  { "bt_rolling_join_", (DL_FUNC) &bt_rolling_join_, 9 },
  { "bt_expr_",         (DL_FUNC) &bt_expr_,         5 },
  { "bt_rbind_",        (DL_FUNC) &bt_rbind_,        4 },
  { NULL, NULL, 0 }
};

attribute_visible void R_init_basetable(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
  bt_init_altrep(dll);
}

} // extern "C"
