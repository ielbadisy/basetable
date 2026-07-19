rangemerge <- function(x, y, by, lower, upper, value) {
  x_dt <- bt_as_data_table_ro(x)
  y_dt <- bt_as_data_table_ro(y)
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

  x_tmp <- data.table::copy(x_dt)
  y_tmp <- data.table::copy(y_dt)
  value_end <- "__bt_value_end"
  y_tmp[[value_end]] <- y_tmp[[value]]
  data.table::setkeyv(y_tmp, c(by, value, value_end))

  out <- data.table::foverlaps(
    x_tmp,
    y_tmp,
    by.x = c(by, lower, upper),
    by.y = data.table::key(y_tmp),
    type = "any",
    nomatch = NA
  )
  out[[value_end]] <- NULL
  out[]
}
