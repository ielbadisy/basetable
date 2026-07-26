#' Map over vectors and lists
#'
#' Small base-R-style mapping helpers with no external dependency.
#'
#' @param .x A vector or list.
#' @param .f A function or function name.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return `map()` returns a list. `walk()` returns `.x` invisibly.
#' @export
map <- function(.x, .f, ...) {
  f <- match.fun(.f)
  lapply(.x, function(x) f(x, ...))
}

#' Map over a list of arguments
#'
#' @param .l A list of vectors or lists with a common length.
#' @param .f A function or function name.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return `pmap()` returns a list. `pwalk()` returns `.l` invisibly.
#' @export
pmap <- function(.l, .f, ...) {
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

#' @rdname map
#' @export
walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}

#' @rdname map
#' @export
pwalk <- function(.l, .f, ...) {
  pmap(.l, .f, ...)
  invisible(.l)
}

#' Reduce a vector or list
#'
#' Reduce a vector or list using base R's `Reduce()`.
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
