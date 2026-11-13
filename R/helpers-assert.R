#' Validation and schema helpers
#'
#' Low-level helpers for validating columns and keys or comparing schemas.
#'
#' @param data,x,y A `data.frame`.
#' @param cols Column names or selectors, as accepted by the verbs.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return `assertcols()` and `assertkey()` return `data` invisibly or signal an
#'   error; `commonnames()` returns the shared column names; `duplicatedkeys()`
#'   returns the key combinations that occur more than once.
#' @examples
#' commonnames(mtcars, iris)
#' assertcols(mtcars, c("mpg", "hp"))
#' duplicatedkeys(data.frame(id = c(1, 1, 2)), "id")
#' @name assertcols
#' @export
assertcols <- function(data, cols) {
  bt_resolve_cols(bt_as_data_frame(data), cols)
  invisible(data)
}

#' @rdname assertcols
#' @export
commonnames <- function(x, y) {
  intersect(names(bt_as_data_frame(x)), names(bt_as_data_frame(y)))
}

#' @rdname assertcols
#' @export
duplicatedkeys <- function(data, by) {
  out <- count(data, by = by, sort = FALSE, name = "N")
  bt_engine_subset(out, rows = out$N > 1L)
}

#' @rdname assertcols
#' @export
assertkey <- function(data, by) {
  dup <- duplicatedkeys(data, by)
  if (nrow(dup) > 0L) {
    stop("`by` does not identify unique rows.", call. = FALSE)
  }
  invisible(data)
}
