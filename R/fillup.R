fillup <- function(data, cols, by = NULL) {
  dt <- bt_as_data_table(data)
  cols <- bt_resolve_cols(dt, cols)
  by <- if (is.null(by)) character(0) else bt_resolve_cols(dt, by)

  if (length(cols) < 1L) {
    stop("`cols` must contain at least one column.", call. = FALSE)
  }

  if (length(by) == 0L) {
    dt[, (cols) := lapply(.SD, bt_nocb), .SDcols = cols]
  } else {
    dt[, (cols) := lapply(.SD, bt_nocb), by = by, .SDcols = cols]
  }

  bt_as_tibble(dt)
}
