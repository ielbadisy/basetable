summaries <- function(data, by = NULL, ...) {
  dt <- bt_as_data_table_ro(data)
  dots <- as.list(substitute(list(...)))[-1L]

  if (length(dots) == 0L) {
    stop("At least one summary expression is required.", call. = FALSE)
  }

  nms <- names(dots)
  if (is.null(nms) || any(nms == "")) {
    stop("All summary expressions must be named.", call. = FALSE)
  }

  by <- if (is.null(by)) character(0) else bt_resolve_cols(dt, by)
  j_call <- as.call(c(quote(list), dots))

  out <- if (length(by) == 0L) {
    dt[, eval(j_call)]
  } else {
    dt[, eval(j_call), keyby = by]
  }

  expected_groups <- if (length(by) == 0L) 1L else data.table::uniqueN(dt, by = by)
  if (nrow(out) != expected_groups) {
    stop("Each summary expression must return one value.", call. = FALSE)
  }

  bt_as_tibble(out)
}
