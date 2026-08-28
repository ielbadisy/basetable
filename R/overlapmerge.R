overlapmerge <- function(x, y, startx, endx, starty, endy, by = NULL) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)

  by <- if (is.null(by)) character(0) else bt_resolve_cols(x_dt, by)
  if (length(by) > 0L) bt_resolve_cols(y_dt, by)
  startx <- bt_resolve_cols(x_dt, startx)
  endx <- bt_resolve_cols(x_dt, endx)
  starty <- bt_resolve_cols(y_dt, starty)
  endy <- bt_resolve_cols(y_dt, endy)

  if (length(startx) != 1L || length(endx) != 1L || length(starty) != 1L || length(endy) != 1L) {
    stop("`startx`, `endx`, `starty`, and `endy` must each name one column.", call. = FALSE)
  }

  bt_range_join(
    x_dt, y_dt,
    by = by,
    predicates = list(
      list(x = startx, op = "<=", y = endy),
      list(x = endx, op = ">=", y = starty)
    ),
    y_cols = setdiff(names(y_dt), by),
    all.x = TRUE
  )
}
