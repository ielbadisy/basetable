summaries <- function(data, by = NULL, ...) {
  df <- bt_as_data_frame(data)
  dots <- as.list(substitute(list(...)))[-1L]

  if (length(dots) == 0L) {
    stop("At least one summary expression is required.", call. = FALSE)
  }

  nms <- names(dots)
  if (is.null(nms) || any(nms == "")) {
    stop("All summary expressions must be named.", call. = FALSE)
  }

  by <- if (is.null(by)) character(0) else bt_resolve_cols(df, by)
  pieces <- if (length(by) == 0L) list(df) else split(df, by = by, keep.by = TRUE)

  rows <- lapply(pieces, function(piece) {
    env <- list2env(as.list(piece), parent = parent.frame())
    vals <- lapply(dots, function(expr) eval(expr, env, parent.frame()))

    if (any(vapply(vals, length, integer(1)) != 1L)) {
      stop("Each summary expression must return one value.", call. = FALSE)
    }

    out <- as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
    names(out) <- nms

    if (length(by) > 0L) {
      out <- cbind(piece[1L, by, drop = FALSE], out, stringsAsFactors = FALSE)
    }

    out
  })

  bt_as_tibble(data.table::rbindlist(rows, fill = TRUE))
}
