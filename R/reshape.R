reshape <- function(data, ..., direction) {
  bt_as_data_table(stats::reshape(bt_as_data_frame(data), ..., direction = direction))
}

#' Stack columns
#'
#' Convert selected columns into long format with an indicator column.
#' @param data A data frame or data table.
#' @param select Optional character vector of columns to stack.
#' @param drop Whether to drop rows with missing values.
#' @param ... Arguments of [utils::stack()] only.
#' @details basetable's `stack()` and `unstack()` mask the [utils::stack()]
#'   versions when attached; a list, a vector or an argument named `x` goes to
#'   [utils::stack()] / [utils::unstack()].
#' @return A basetable with a stacked value column.
#' @export
stack <- function(data, select = NULL, drop = FALSE, ...) {
  if (...length() > 0L || "x" %in% names(sys.call()) || !is.data.frame(data)) {
    return(bt_call_base(sys.call(), quote(utils::stack), parent.frame(), c(data = "x")))
  }
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
  if ("x" %in% names(sys.call()) || !is.data.frame(data)) {
    return(bt_call_base(sys.call(), quote(utils::unstack), parent.frame(), c(data = "x")))
  }
  bt_as_data_table(utils::unstack(bt_as_data_frame(data), form = form, ...))
}
