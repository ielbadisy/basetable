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
