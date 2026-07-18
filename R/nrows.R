#' Number of rows
#'
#' Return the number of rows in a table.
#'
#' @param data A data frame or data table.
#'
#' @return An integer scalar.
#' @export
nrows <- function(data) {
  nrow(bt_as_data_frame(data))
}
