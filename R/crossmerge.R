#' Cartesian join two tables
#'
#' Return every combination of rows from \code{x} and \code{y}.
#' @param x,y Data frames or data tables to combine.
#' @return A basetable containing the Cartesian product of the rows.
#' @export
crossmerge <- function(x, y) {
  x_df <- bt_as_data_frame(x)
  y_df <- bt_as_data_frame(y)
  bt_join_rows(x_df, y_df, by = character(0))
}
