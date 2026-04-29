pick <- function(data, cols) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  bt_as_tibble(df[, cols, drop = FALSE])
}

drop <- function(data, cols) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  keep <- setdiff(names(df), cols)
  bt_as_tibble(df[, keep, drop = FALSE])
}
