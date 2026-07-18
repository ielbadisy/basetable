rangemerge <- function(x, y, by, lower, upper) {
  x_dt <- bt_as_data_table_ro(x)
  y_dt <- bt_as_data_table_ro(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  lower <- bt_resolve_cols(x_dt, lower)
  upper <- bt_resolve_cols(x_dt, upper)

  if (length(by) < 1L) {
    stop("`by` must contain at least one column.", call. = FALSE)
  }
  if (length(lower) != 1L || length(upper) != 1L) {
    stop("`lower` and `upper` must each name one column.", call. = FALSE)
  }

  data.table::merge.data.table(x_dt, y_dt, by = by, all.x = TRUE, sort = FALSE)
}
