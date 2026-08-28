rollingmerge <- function(x, y, by, direction = c("backward", "forward", "nearest"), tolerance = Inf) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)

  if (length(by) < 1L) {
    stop("`by` must contain at least one column.", call. = FALSE)
  }

  direction <- match.arg(direction)
  roll_key <- by[[length(by)]]
  exact <- setdiff(by, roll_key)

  bt_rolling_join(
    x_dt, y_dt,
    exact = exact,
    x_roll = roll_key,
    y_roll = roll_key,
    direction = direction,
    tolerance = as.numeric(tolerance),
    y_cols = setdiff(names(y_dt), by)
  )
}
