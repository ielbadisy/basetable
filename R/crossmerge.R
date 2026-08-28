crossmerge <- function(x, y) {
  x_df <- bt_as_data_frame(x)
  y_df <- bt_as_data_frame(y)
  bt_join_rows(x_df, y_df, by = character(0))
}
