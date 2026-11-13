#' Return rows with missing values
#'
#' Return rows where selected columns are missing or blank.
#' @param data A data frame or data table.
#' @param cols Optional character vector of columns to inspect.
#' @param mode Whether any or all selected columns must be missing.
#' @return A basetable containing the matching rows.
#' @export
missingrows <- function(data, cols = NULL, mode = c("any", "all")) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols, allow_null = TRUE)
  mode <- match.arg(mode)

  if (length(cols) == 0L) {
    return(bt_as_data_table(df[0, , drop = FALSE]))
  }

  miss <- do.call(cbind, lapply(df[cols], bt_is_blank))
  if (is.null(dim(miss))) {
    miss <- matrix(miss, ncol = 1L)
  }

  keep <- switch(
    mode,
    any = rowSums(miss) > 0L,
    all = rowSums(miss) == ncol(miss)
  )

  bt_as_data_table(df[keep, , drop = FALSE])
}
