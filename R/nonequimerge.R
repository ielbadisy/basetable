nonequimerge <- function(x, y, by, ...) {
  x_dt <- bt_as_data_table(x)
  y_dt <- bt_as_data_table(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)

  data.table::merge.data.table(x_dt, y_dt, by = by, all = FALSE, sort = FALSE, ...)
}
