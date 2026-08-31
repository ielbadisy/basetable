#' Map over vectors and lists
#'
#' A small base-R-style mapping helper with no external dependency: apply `.f`
#' to each element of `.x` and return the results in a list, like
#' [base::lapply()] with a friendlier argument name.
#'
#' @param .x A vector or list.
#' @param .f A function or function name.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return `map()` returns a list.
#' @examples
#' map(1:3, function(x) x + 1)
#' @export
map <- function(.x, .f, ...) {
  f <- match.fun(.f)
  lapply(.x, function(x) f(x, ...))
}

#' Traverse a list of arguments
#'
#' Call `.f` once per position across several equal-length vectors or lists,
#' passing the i-th element of each as a positional argument. The parallel-map
#' companion to [map()].
#'
#' @param .l A list of vectors or lists with a common length.
#' @param .f A function or function name.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return `traverse()` returns a list.
#' @examples
#' traverse(list(a = 1:2, b = 10:11), function(a, b) a + b)
#' @export
traverse <- function(.l, .f, ...) {
  if (!is.list(.l)) {
    stop("`.l` must be a list.", call. = FALSE)
  }

  lens <- vapply(.l, length, integer(1))
  if (length(lens) == 0L) {
    return(list())
  }
  if (length(unique(lens)) != 1L) {
    stop("All elements of `.l` must have the same length.", call. = FALSE)
  }

  f <- match.fun(.f)
  n <- lens[[1L]]
  lapply(seq_len(n), function(i) {
    args <- lapply(.l, function(x) x[[i]])
    do.call(f, c(args, list(...)))
  })
}

bt_fold <- function(.x, .f, .init, .right, .accumulate, .simplify) {
  out <- if (is.null(.init)) {
    base::Reduce(
      f = match.fun(.f),
      x = .x,
      right = .right,
      accumulate = .accumulate
    )
  } else {
    base::Reduce(
      f = match.fun(.f),
      x = .x,
      init = .init,
      right = .right,
      accumulate = .accumulate
    )
  }

  if (!.accumulate || !.simplify) {
    return(out)
  }

  tryCatch(base::simplify2array(out), error = function(e) out)
}

#' Fold a vector or list from the right
#'
#' Reduce `.x` with the two-argument function `.f`, associating from the right,
#' a thin wrapper around [base::Reduce()] with `right = TRUE`. With
#' `.accumulate = TRUE` the intermediate results are returned as well.
#'
#' @param .x A vector or list.
#' @param .f A two-argument reducing function.
#' @param .init Optional initial value.
#' @param .accumulate Return intermediate accumulated values.
#' @param .simplify Simplify accumulated results when possible.
#'
#' @return The folded value, or accumulated values when `.accumulate = TRUE`.
#' @examples
#' foldr(1:4, `+`)
#' foldr(letters[1:4], paste0, .accumulate = TRUE)
#' @export
foldr <- function(.x, .f, .init = NULL, .accumulate = FALSE, .simplify = TRUE) {
  bt_fold(.x, .f, .init = .init, .right = TRUE, .accumulate = .accumulate, .simplify = .simplify)
}
