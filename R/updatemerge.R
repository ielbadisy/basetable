updatemerge <- function(x, y, by, cols = NULL) {
  x_dt <- bt_as_data_table(x)
  y_dt <- bt_as_data_table(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)

  if (is.null(cols)) {
    cols <- setdiff(intersect(names(x_dt), names(y_dt)), by)
  }
  cols <- bt_resolve_cols(x_dt, cols)

  y_first <- y_dt[!duplicated(y_dt, by = by)]
  idx <- y_first[x_dt, on = by, which = TRUE]

  for (nm in cols) {
    values <- y_first[[nm]][idx]
    keep <- !is.na(idx)
    x_dt[[nm]][keep] <- values[keep]
  }

  x_dt
}
