#' Split a table by groups
#'
#' Split a table into pieces by one or more grouping columns. When \code{data}
#' is not a data frame, dispatches to \code{base::split()} so the classic
#' \code{split(vector, factor)} idiom keeps working with basetable attached.
#' @param data A data frame or data table, or an atomic vector.
#' @param by For a data frame, a character vector of grouping columns. For an atomic vector, a factor (or object coercible to one) the same length as \code{data}, as in \code{base::split()}.
#' @param drop Whether to drop empty groups.
#' @param keep.by Whether to keep grouping columns in each piece. Ignored when \code{data} is not a data frame.
#' @return A named list of basetables (data frame input) or a named list of vectors
#' (atomic vector input, matching \code{base::split()}).
#' @export
split <- function(data, by, drop = FALSE, keep.by = TRUE) {
  if (!is.data.frame(data)) {
    return(base::split(x = data, f = by, drop = drop))
  }
  bt_split_by(data, by = by, drop = drop, keepby = keep.by)
}
