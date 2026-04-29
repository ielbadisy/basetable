#' Map over vectors and lists
#'
#' Lightweight map helpers backed by the `functionals` package.
#'
#' @param .x A vector or list.
#' @param .f A function or function name.
#' @param ... Additional arguments passed to `.f`.
#' @param ncores,pb Passed through to `functionals` mapping helpers.
#'
#' @return `map()` returns a list. Typed variants return an atomic vector.
#'   `walk()` returns `.x` invisibly.
#' @export
map <- function(.x, .f, ..., ncores = NULL, pb = FALSE) {
  functionals::fmap(.x, .f, ncores = ncores, pb = pb, ...)
}

#' @rdname map
#' @export
map_lgl <- function(.x, .f, ..., ncores = NULL, pb = FALSE) {
  vapply(map(.x, .f, ..., ncores = ncores, pb = pb), function(x) x, logical(1))
}

#' @rdname map
#' @export
map_int <- function(.x, .f, ..., ncores = NULL, pb = FALSE) {
  vapply(map(.x, .f, ..., ncores = ncores, pb = pb), function(x) x, integer(1))
}

#' @rdname map
#' @export
map_dbl <- function(.x, .f, ..., ncores = NULL, pb = FALSE) {
  vapply(map(.x, .f, ..., ncores = ncores, pb = pb), function(x) x, numeric(1))
}

#' @rdname map
#' @export
map_chr <- function(.x, .f, ..., ncores = NULL, pb = FALSE) {
  vapply(map(.x, .f, ..., ncores = ncores, pb = pb), function(x) x, character(1))
}

#' @rdname map
#' @export
walk <- function(.x, .f, ..., ncores = NULL, pb = FALSE) {
  functionals::fwalk(.x, .f, ncores = ncores, pb = pb, ...)
  invisible(.x)
}

#' Reduce a vector or list
#'
#' Reduce a vector or list using `functionals::freduce()`.
#'
#' @param .x A vector or list.
#' @param .f A two-argument reducing function.
#' @param .init Optional initial value.
#' @param .right Reduce from the right.
#' @param .accumulate Return intermediate accumulated values.
#' @param .simplify Simplify accumulated results when possible.
#'
#' @return The reduced value, or accumulated values when `.accumulate = TRUE`.
#' @export
reduce <- function(.x, .f, .init = NULL, .right = FALSE, .accumulate = FALSE, .simplify = TRUE) {
  functionals::freduce(
    .x = .x,
    .f = .f,
    .init = .init,
    .right = .right,
    .accumulate = .accumulate,
    .simplify = .simplify
  )
}
