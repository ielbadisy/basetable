rangemerge <- function(x, y, by, lower, upper, value) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  lower <- bt_resolve_cols(x_dt, lower)
  upper <- bt_resolve_cols(x_dt, upper)
  value <- bt_resolve_cols(y_dt, value)

  if (length(by) < 1L) {
    stop("`by` must contain at least one column.", call. = FALSE)
  }
  if (length(lower) != 1L || length(upper) != 1L) {
    stop("`lower` and `upper` must each name one column.", call. = FALSE)
  }
  if (length(value) != 1L) {
    stop("`value` must name one column.", call. = FALSE)
  }

  bt_range_join(
    x_dt, y_dt,
    by = by,
    predicates = list(
      list(x = lower, op = "<=", y = value),
      list(x = upper, op = ">=", y = value)
    ),
    y_cols = setdiff(names(y_dt), by),
    all.x = TRUE
  )
}
