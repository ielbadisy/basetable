#' Deprecated functions
#'
#' These functions were renamed to glued lowercase names and will be removed in
#' a future release. The deprecation warning is given only when an old name is
#' called directly from the global environment (soft deprecation), so packages
#' that still use the old names keep working silently until they update.
#'
#' @param x,data,y,... Arguments passed to the new function.
#' @param cols,by Arguments passed to the new function.
#' @name basetable-deprecated
#' @keywords internal
NULL

#' @rdname basetable-deprecated
#' @export
as_basetable <- function(x, ...) {
  softdeprecated("asbasetable", parent.frame())
  asbasetable(x, ...)
}

#' @rdname basetable-deprecated
#' @export
is_basetable <- function(x) {
  softdeprecated("isbasetable", parent.frame())
  isbasetable(x)
}

#' @rdname basetable-deprecated
#' @export
assert_cols <- function(data, cols) {
  softdeprecated("assertcols", parent.frame())
  assertcols(data, cols)
}

#' @rdname basetable-deprecated
#' @export
assert_key <- function(data, by) {
  softdeprecated("assertkey", parent.frame())
  assertkey(data, by)
}

#' @rdname basetable-deprecated
#' @export
common_names <- function(x, y) {
  softdeprecated("commonnames", parent.frame())
  commonnames(x, y)
}

#' @rdname basetable-deprecated
#' @export
duplicated_keys <- function(data, by) {
  softdeprecated("duplicatedkeys", parent.frame())
  duplicatedkeys(data, by)
}

softdeprecated <- function(new, env) {
  if (identical(topenv(env), globalenv())) {
    old <- as.character(sys.call(-1L)[[1L]])
    .Deprecated(new, package = "basetable", old = old[length(old)])
  }
  invisible(NULL)
}
