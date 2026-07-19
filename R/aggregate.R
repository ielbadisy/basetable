aggregate <- function(data, by, value = NULL, fun, ..., na.rm = FALSE, sort = TRUE) {
  dt <- bt_as_data_table(data)
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
  df <- bt_as_data_frame(data)
  by <- bt_resolve_cols(df, by)
  key <- interaction(df[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  tab <- as.data.frame(table(key), stringsAsFactors = FALSE)
  tab <- tab[tab$Freq > 0L, , drop = FALSE]
  rows <- match(as.character(tab$key), as.character(key))
  out <- cbind(df[rows, by, drop = FALSE], tab["Freq"])
  names(out)[ncol(out)] <- name
  rownames(out) <- NULL
  if (sort) {
    out <- out[order(out[[name]], decreasing = TRUE), , drop = FALSE]
    rownames(out) <- NULL
  }
  bt_as_tibble(out)
}
