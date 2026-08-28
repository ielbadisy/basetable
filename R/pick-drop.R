pick <- function(data, cols) {
  bt_engine_subset(data, cols = cols)
}

drop <- function(data, cols) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  keep <- setdiff(names(df), cols)
  bt_engine_subset(df, cols = keep)
}
