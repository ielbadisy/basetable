antimerge <- function(x, y, by) {
  x_dt <- bt_as_data_table_ro(x)
  y_dt <- bt_as_data_table_ro(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)

  if (length(by) < 1L) {
    stop("`by` must contain at least one column.", call. = FALSE)
  }

  y_keys <- unique(y_dt[, by, with = FALSE])
  x_dt[!y_keys, on = by]
}
