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
  j_call <- as.call(c(quote(list), dots))

  if (length(by) == 0L) {
    values <- eval(j_call, envir = bt_data_mask(df, parent.frame()), enclos = parent.frame())
    if (any(vapply(values, length, integer(1)) != 1L)) {
      stop("Each summary expression must return one value.", call. = FALSE)
    }
    return(bt_as_data_table(as.data.frame(values, stringsAsFactors = FALSE)))
  }

  group_info <- bt_engine_groups(df, by)
  groups <- bt_group_rows(group_info$id)
  keys <- bt_engine_subset(df, rows = group_info$first, cols = by)
  rows <- lapply(groups, function(idx) {
    values <- eval(j_call, envir = bt_data_mask(df[idx, , drop = FALSE], parent.frame()), enclos = parent.frame())
    if (any(vapply(values, length, integer(1)) != 1L)) {
      stop("Each summary expression must return one value.", call. = FALSE)
    }
    as.data.frame(values, stringsAsFactors = FALSE)
  })
  vals <- do.call(rbind, rows)
  row.names(vals) <- NULL
  out <- cbind(keys, vals)
  bt_as_data_table(out)
}
