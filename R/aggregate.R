aggregate <- function(data, by, value = NULL, fun, ..., na.rm = FALSE, sort = TRUE) {
  dt <- bt_as_data_table_ro(data)
  by <- bt_resolve_cols(dt, by)

  if (is.null(value)) {
    value <- setdiff(names(dt), by)
  } else {
    value <- bt_resolve_cols(dt, value)
  }

  f <- match.fun(fun)
  out <- dt[, lapply(.SD, function(x) f(x, ..., na.rm = na.rm)), by = by, .SDcols = value]

  if (sort && length(by) > 0L) {
    data.table::setorderv(out, by)
  }

  out
}

count <- function(data, by, sort = TRUE, name = "n") {
  dt <- bt_as_data_table_ro(data)
  by <- bt_resolve_cols(dt, by)
  out <- dt[, list(N = .N), by = by]
  data.table::setnames(out, "N", name)
  if (sort) {
    data.table::setorderv(out, name, order = -1L)
  }
  bt_as_tibble(out)
}
