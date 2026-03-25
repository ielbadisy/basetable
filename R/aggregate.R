aggregate <- function(data, by, value = NULL, fun, ..., na.rm = FALSE, sort = TRUE) {
  df <- bt_as_data_frame(data)
  by <- bt_resolve_cols(df, by)

  if (is.null(value)) {
    value <- setdiff(names(df), by)
  } else {
    value <- bt_resolve_cols(df, value)
  }

  f <- match.fun(fun)
  key <- interaction(df[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  pieces <- base::split(df[, c(by, value), drop = FALSE], key, drop = TRUE)

  out <- lapply(pieces, function(piece) {
    vals <- lapply(piece[, value, drop = FALSE], function(x) f(x, ..., na.rm = na.rm))
    vals_df <- as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
    names(vals_df) <- value
    cbind(piece[1L, by, drop = FALSE], vals_df, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL

  if (sort && length(by) > 0L && all(by %in% names(out))) {
    ord <- do.call(order, lapply(by, function(nm) out[[nm]]))
    out <- out[ord, , drop = FALSE]
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
  out
}
