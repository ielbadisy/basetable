#' Select columns
#'
#' Select columns by name.
#' @param data A data frame or data table.
#' @param cols Character vector of column names.
#' @return A basetable containing the selected columns.
#' @export
pick <- function(data, cols) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  bt_as_data_table(df[cols])
}

#' Drop columns
#'
#' Drop columns by name.
#' @param data A data frame or data table.
#' @param cols Character vector of column names to remove.
#' @return A basetable with the selected columns removed.
#' @export
drop <- function(data, cols) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  keep <- setdiff(names(df), cols)
  bt_as_data_table(df[keep])
}
