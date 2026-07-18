updatemerge <- function(x, y, by, cols = NULL) {
  x_dt <- bt_as_data_table(x)
  y_dt <- bt_as_data_table(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)

  if (is.null(cols)) {
    cols <- setdiff(intersect(names(x_dt), names(y_dt)), by)
  }
  cols <- bt_resolve_cols(x_dt, cols)

  x_key <- interaction(x_dt[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  y_key <- interaction(y_dt[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  idx <- match(x_key, y_key)

  for (nm in cols) {
    values <- y_dt[[nm]][idx]
    keep <- !is.na(idx)
    x_dt[[nm]][keep] <- values[keep]
  }

  x_dt
}
