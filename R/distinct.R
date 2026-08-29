#' Distinct rows, in memory or straight off a delimited file
#'
#' With a data frame, `distinct()` is [uniquerows()]: the unique rows, keeping
#' only `cols` when given. With a single file path as `data`, it is a fused
#' one-pass scan that returns the distinct combinations of `cols` without ever
#' materialising the file as R vectors (see [aggregate()]'s file mode).
#'
#' @param data A data frame, or a single path to a delimited text file.
#' @param cols Columns whose distinct values (or combinations) to return.
#'   `NULL` (the default) uses every column.
#' @param ... For the in-memory form, passed to [uniquerows()] (e.g.
#'   `.keep_all`); for the file form, passed to the file reader (`where`,
#'   `delim`, `n_threads`, ...).
#'
#' @return A `basetable`.
#' @export
distinct <- function(data, cols = NULL, ...) {
  if (is.character(data) && length(data) == 1L) {
    if (is.null(cols)) {
      stop("`distinct()` on a file needs `cols`.", call. = FALSE)
    }
    return(distinct_from_file(data, cols = cols, ...))
  }
  uniquerows(data, cols = cols, ...)
}
