assert_cols <- function(data, cols) {
  bt_resolve_cols(bt_as_data_frame(data), cols)
  invisible(data)
}

common_names <- function(x, y) {
  intersect(names(bt_as_data_frame(x)), names(bt_as_data_frame(y)))
}

duplicated_keys <- function(data, by) {
  dt <- bt_as_data_table_ro(data)
  by <- bt_resolve_cols(dt, by)
  out <- dt[, list(N = .N), by = by][N > 1L]
  bt_as_tibble(out)
}

assert_key <- function(data, by) {
  dup <- duplicated_keys(data, by)
  if (nrow(dup) > 0L) {
    stop("`by` does not identify unique rows.", call. = FALSE)
  }
  invisible(data)
}
