nonequimerge <- function(x, y, by, ...) {
  x_dt <- bt_as_data_table_ro(x)
  y_dt <- bt_as_data_table_ro(y)

  is_condition <- grepl("[<>=!]", by)
  plain <- by[!is_condition]
  if (length(plain) > 0L) {
    bt_resolve_cols(x_dt, plain)
    bt_resolve_cols(y_dt, plain)
  }

  x_dt[y_dt, on = by, nomatch = NULL, allow.cartesian = TRUE, ...]
}
