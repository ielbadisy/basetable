#' Number of columns
#'
#' Return the number of columns in a table.
#'
#' @param data A data frame or data table.
#'
#' @return An integer scalar.
#' @export
ncols <- function(data) {
  ncol(bt_as_data_frame(data))
}
