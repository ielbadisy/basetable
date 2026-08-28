fillup <- function(data, cols, by = NULL) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  by <- if (is.null(by)) character(0) else bt_resolve_cols(df, by)

  if (length(cols) < 1L) {
    stop("`cols` must contain at least one column.", call. = FALSE)
  }

  if (length(by) == 0L) {
    for (nm in cols) df[[nm]] <- bt_nocb(df[[nm]])
  } else {
    groups <- bt_group_rows(bt_engine_groups(df, by)$id)
    for (nm in cols) {
      for (idx in groups) df[[nm]][idx] <- bt_nocb(df[[nm]][idx])
    }
  }

  bt_as_data_table(df)
}
