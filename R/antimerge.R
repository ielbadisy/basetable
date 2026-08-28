antimerge <- function(x, y, by) {
  x_df <- bt_as_data_frame(x)
  y_df <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_df, by)
  bt_resolve_cols(y_df, by)

  if (length(by) < 1L) {
    stop("`by` must contain at least one column.", call. = FALSE)
  }

  bt_engine_subset(x_df, rows = !bt_engine_match_mask(x_df, y_df, by))
}
