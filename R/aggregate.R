aggregate <- function(data, by, value = NULL, fun, ..., na.rm = FALSE, sort = TRUE) {
  dt <- bt_as_data_table_ro(data)
  by <- bt_resolve_cols(dt, by)

  if (is.null(value)) {
    value <- setdiff(names(dt), by)
  } else {
    value <- bt_resolve_cols(dt, value)
  }

  f <- match.fun(fun)
  f_args <- names(formals(args(f)))
  forward_na_rm <- "na.rm" %in% f_args || "..." %in% f_args
  out <- if (forward_na_rm) {
    dt[, lapply(.SD, function(x) f(x, ..., na.rm = na.rm)), by = by, .SDcols = value]
  } else {
    dt[, lapply(.SD, function(x) f(x, ...)), by = by, .SDcols = value]
  }

  if (sort && length(by) > 0L) {
    data.table::setorderv(out, by)
  }

  out
}

count <- function(data, by, sort = TRUE, name = "n") {
  out <- bt_engine_count(data, by = by, name = name)
  if (sort) {
    out <- bt_engine_order(out, by = name, decreasing = TRUE)
  }
  out
}
