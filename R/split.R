#' Split a table by groups
#'
#' Split a table into pieces by one or more grouping columns. When \code{data}
#' is not a data frame, dispatches to \code{base::split()} so the classic
#' \code{split(vector, factor)} idiom keeps working with basetable attached.
#' @param data A data frame or data table, or an atomic vector.
#' @param by For a data frame, a character vector of grouping columns. For an atomic vector, a factor (or object coercible to one) the same length as \code{data}, as in \code{base::split()}.
#' @param drop Whether to drop empty groups.
#' @param keep.by Whether to keep grouping columns in each piece. Ignored when \code{data} is not a data frame.
#' @param ... Arguments of \code{base::split()} only (\code{sep}, \code{lex.order}).
#' @details basetable's \code{split()} masks \code{base::split()} when attached.
#'   The call goes to \code{base::split()} unless \code{data} is a data frame
#'   and \code{by} names its columns, so \code{split(df, df$g)} keeps its base
#'   meaning.
#' @return A named list of basetables (data frame input) or a named list of vectors
#' (atomic vector input, matching \code{base::split()}).
#' @export
split <- function(data, by, drop = FALSE, keep.by = TRUE, ...) {
  if (...length() > 0L || any(c("x", "f") %in% names(sys.call())) || !is.data.frame(data) ||
      !is.character(by) || !all(by %in% names(data))) {
    return(bt_call_base(sys.call(), quote(base::split), parent.frame(), c(data = "x", by = "f")))
  }
  bt_split_by(data, by = by, drop = drop, keepby = keep.by)
}
