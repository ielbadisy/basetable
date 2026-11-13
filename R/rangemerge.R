#' Merge two tables by a range condition
#'
#' For each row of \code{x}, keep the row(s) of \code{y} whose \code{value}
#' column falls between \code{x}'s \code{lower} and \code{upper} bounds
#' (inclusive), within the exact-match \code{by} key.
#' @param x,y Data frames or data tables to join.
#' @param by Character vector of exact-match join columns, present in both \code{x} and \code{y}.
#' @param lower,upper Single columns in \code{x} describing the inclusive range bounds to test against.
#' @param value Single column in \code{y} whose value is tested against each matching \code{x} row's \verb{[lower, upper]} range.
#' @return A basetable with one row per \code{x} row for each \code{y} row whose
#' \code{value} falls within \verb{[lower, upper]} (matching on \code{by}); an
#' \code{x} row with no matching \code{y} row appears once with \code{NA} for
#' \code{y}'s columns.
#' @examples
#' x <- data.frame(id = c(1, 1, 2), lower = c(0, 0, 0), upper = c(10, 10, 20))
#' y <- data.frame(id = c(1, 1, 2), val = c(5, 15, 5), label = c("a", "b", "c"))
#' rangemerge(x, y, by = "id", lower = "lower", upper = "upper", value = "val")
#' @export
rangemerge <- function(x, y, by, lower, upper, value) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  lower <- bt_resolve_cols(x_dt, lower)
  upper <- bt_resolve_cols(x_dt, upper)
  value <- bt_resolve_cols(y_dt, value)

  if (length(by) < 1L) {
    stop("`by` must contain at least one column.", call. = FALSE)
  }
  if (length(lower) != 1L || length(upper) != 1L) {
    stop("`lower` and `upper` must each name one column.", call. = FALSE)
  }
  if (length(value) != 1L) {
    stop("`value` must name one column.", call. = FALSE)
  }

  bt_range_join(
    x_dt, y_dt,
    by = by,
    predicates = list(
      list(x = lower, op = "<=", y = value),
      list(x = upper, op = ">=", y = value)
    ),
    y_cols = setdiff(names(y_dt), by),
    all.x = TRUE
  )
}
