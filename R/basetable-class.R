# The basetable result class.
#
# Every verb returns a `basetable`: a data.frame carrying basetable's own
# class so that printing is compact and `[` keeps the class without dropping
# to a vector. There is no data.table machinery; `as.data.frame()` strips back
# to an ordinary frame. The native engine stamps this class directly (see
# set_table_class in src/bt_engine.cpp); `new_basetable()` is the R-side path.

new_basetable <- function(x) {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
  }
  x <- unclass(x)
  attr(x, "row.names") <- if (length(x) == 0L) {
    integer(0)
  } else {
    c(NA_integer_, -length(x[[1L]]))
  }
  class(x) <- c("basetable", "data.frame")
  x
}

#' Coerce an object to a basetable
#'
#' `as_basetable()` turns a data frame, a list of equal-length columns, a
#' matrix, or anything else [as.data.frame()] accepts into a `basetable`: an
#' ordinary data frame carrying basetable's lightweight class, the same object
#' every basetable verb returns. Use it when a package or script wants to hand
#' its own data to basetable explicitly, rather than relying on a verb to stamp
#' the class.
#'
#' The data are left unchanged; only the class and the `row.names` attribute
#' are normalised. An object that is already a `basetable` is returned as-is,
#' and [as.data.frame()] strips the class again.
#'
#' @param x An object to coerce.
#' @param ... Passed to [as.data.frame()] by the default method; ignored
#'   otherwise.
#'
#' @return A `basetable`: a data frame with the `basetable` class.
#' @seealso [is_basetable()]
#' @export
#' @examples
#' bt <- as_basetable(data.frame(g = c("a", "b"), x = 1:2))
#' class(bt)
#' identical(as_basetable(bt), bt)
#' as_basetable(list(g = c("a", "b"), x = 1:2))
as_basetable <- function(x, ...) {
  UseMethod("as_basetable")
}

#' @rdname as_basetable
#' @export
as_basetable.basetable <- function(x, ...) x

#' @rdname as_basetable
#' @export
as_basetable.data.frame <- function(x, ...) new_basetable(x)

#' @rdname as_basetable
#' @export
as_basetable.default <- function(x, ...) new_basetable(as.data.frame(x, ...))

#' Test whether an object is a basetable
#'
#' @param x An object.
#'
#' @return A single logical.
#' @seealso [as_basetable()]
#' @export
#' @examples
#' is_basetable(as_basetable(mtcars))
#' is_basetable(mtcars)
is_basetable <- function(x) inherits(x, "basetable")

#' @export
print.basetable <- function(x, n = getOption("basetable.print_rows", 10L), ...) {
  nr <- nrow(x)
  nc <- ncol(x)
  cat(sprintf("# basetable: %s x %s\n", format(nr, big.mark = ","), nc))
  body <- as.data.frame(x)
  if (nr == 0L) {
    if (nc > 0L) cat("# columns:", paste(names(x), collapse = ", "), "\n")
    return(invisible(x))
  }
  if (is.finite(n) && nr > n) {
    print(utils::head(body, n), ...)
    cat(sprintf("# %s more rows\n", format(nr - n, big.mark = ",")))
  } else {
    print(body, ...)
  }
  invisible(x)
}

#' @export
`[.basetable` <- function(x, i, j, drop = FALSE) {
  cls <- class(x)
  class(x) <- "data.frame"
  mi <- missing(i)
  mj <- missing(j)
  out <- if (nargs() <= 2L) {
    if (mi) x[] else x[i]
  } else if (mi && mj) {
    x[, , drop = drop]
  } else if (mi) {
    x[, j, drop = drop]
  } else if (mj) {
    x[i, , drop = drop]
  } else {
    x[i, j, drop = drop]
  }
  if (is.data.frame(out) && !identical(class(out), cls)) {
    class(out) <- cls
  }
  out
}

#' @export
as.data.frame.basetable <- function(x, ...) {
  class(x) <- "data.frame"
  x
}

#' @export
as.list.basetable <- function(x, ...) {
  x <- unclass(x)
  attr(x, "row.names") <- NULL
  x
}
