assert_cols <- function(data, cols) {
  bt_resolve_cols(bt_as_data_frame(data), cols)
  invisible(data)
}

common_names <- function(x, y) {
  intersect(names(bt_as_data_frame(x)), names(bt_as_data_frame(y)))
}

duplicated_keys <- function(data, by) {
  out <- count(data, by = by, sort = FALSE, name = "N")
  bt_engine_subset(out, rows = out$N > 1L)
}

assert_key <- function(data, by) {
  dup <- duplicated_keys(data, by)
  if (nrow(dup) > 0L) {
    stop("`by` does not identify unique rows.", call. = FALSE)
  }
  invisible(data)
}
