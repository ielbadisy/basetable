#' Fill missing values upward
#'
#' Fill missing values in selected columns using the next observed value.
#' @param data A data frame or data table.
#' @param cols Character vector of columns to fill.
#' @param by Optional grouping columns.
#' @return A basetable with missing values filled upward.
#' @export
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
