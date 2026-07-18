semimerge <- function(x, y, by) {
  x_dt <- bt_as_data_table_ro(x)
  y_dt <- bt_as_data_table_ro(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)

  y_keys <- unique(y_dt[, by, with = FALSE])
  x_dt[y_keys, on = by, nomatch = NULL]
}
