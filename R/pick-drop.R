pick <- function(data, cols) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  df[, cols, drop = FALSE]
}

drop <- function(data, cols) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  keep <- setdiff(names(df), cols)
  df[, keep, drop = FALSE]
}
