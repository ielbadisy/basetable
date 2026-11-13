#' Deprecated functions
#'
#' These functions were renamed to glued lowercase names and will be removed in
#' a future release.
#'
#' @param x,data,y,... Arguments passed to the new function.
#' @param cols,by Arguments passed to the new function.
#' @name basetable-deprecated
#' @keywords internal
NULL

#' @rdname basetable-deprecated
#' @export
as_basetable <- function(x, ...) {
  .Deprecated("asbasetable")
  asbasetable(x, ...)
}

#' @rdname basetable-deprecated
#' @export
is_basetable <- function(x) {
  .Deprecated("isbasetable")
  isbasetable(x)
}

#' @rdname basetable-deprecated
#' @export
assert_cols <- function(data, cols) {
  .Deprecated("assertcols")
  assertcols(data, cols)
}

#' @rdname basetable-deprecated
#' @export
assert_key <- function(data, by) {
  .Deprecated("assertkey")
  assertkey(data, by)
}

#' @rdname basetable-deprecated
#' @export
common_names <- function(x, y) {
  .Deprecated("commonnames")
  commonnames(x, y)
}

#' @rdname basetable-deprecated
#' @export
duplicated_keys <- function(data, by) {
  .Deprecated("duplicatedkeys")
  duplicatedkeys(data, by)
}
