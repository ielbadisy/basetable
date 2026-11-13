reshape <- function(data, ..., direction) {
  bt_as_data_table(stats::reshape(bt_as_data_frame(data), ..., direction = direction))
}

#' Stack columns
#'
#' Convert selected columns into long format with an indicator column.
#' @param data A data frame or data table.
#' @param select Optional character vector of columns to stack.
#' @param drop Whether to drop rows with missing values.
#' @return A basetable with a stacked value column.
#' @export
stack <- function(data, select = NULL, drop = FALSE) {
  df <- bt_as_data_frame(data)
  if (is.null(select)) {
    select <- names(df)
  }
  select <- bt_resolve_cols(df, select)
  bt_as_data_table(utils::stack(df[, select, drop = drop]))
}

#' Unstack a table
#'
#' Reshape a stacked table back into wide form.
#' @param data A data frame or data table.
#' @param form A formula or equivalent description of the reshape.
#' @param ... Additional arguments passed to the underlying method.
#' @return A basetable reshaped into wide format.
#' @export
unstack <- function(data, form, ...) {
  bt_as_data_table(utils::unstack(bt_as_data_frame(data), form = form, ...))
}
