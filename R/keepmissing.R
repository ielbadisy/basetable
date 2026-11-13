#' Keep rows with missing values
#'
#' Return rows where selected columns are missing or blank.
#' @param data A data frame or data table.
#' @param cols Optional character vector of columns to inspect.
#' @param mode Whether any or all selected columns must be missing.
#' @return A basetable containing the matching rows.
#' @export
keepmissing <- function(data, cols = NULL, mode = c("any", "all")) {
  missingrows(data, cols = cols, mode = mode)
}
