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
#' @details basetable's `drop()` masks [base::drop()] when the package is
#'   attached. A call without `cols`, such as `drop(x)` on a matrix or array,
#'   is passed to [base::drop()], so base code keeps working.
#' @return A basetable with the selected columns removed.
#' @examples
#' drop(mtcars, c("mpg", "cyl"))[1:2, 1:3]
#' drop(matrix(1:3, nrow = 1)) # base::drop()
#' @export
drop <- function(data, cols) {
  if (missing(cols)) {
    return(base::drop(data))
  }
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  keep <- setdiff(names(df), cols)
  bt_as_data_table(df[keep])
}
