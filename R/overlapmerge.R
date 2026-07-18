overlapmerge <- function(x, y, startx, endx, starty, endy, by = NULL) {
  x_dt <- bt_as_data_table(x)
  y_dt <- bt_as_data_table(y)

  by <- if (is.null(by)) character(0) else bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  startx <- bt_resolve_cols(x_dt, startx)
  endx <- bt_resolve_cols(x_dt, endx)
  starty <- bt_resolve_cols(y_dt, starty)
  endy <- bt_resolve_cols(y_dt, endy)

  if (length(startx) != 1L || length(endx) != 1L || length(starty) != 1L || length(endy) != 1L) {
    stop("`startx`, `endx`, `starty`, and `endy` must each name one column.", call. = FALSE)
  }

  x_tmp <- data.table::copy(x_dt)
  y_tmp <- data.table::copy(y_dt)

  x_start_tmp <- "__bt_startx"
  x_end_tmp <- "__bt_endx"

  data.table::setnames(x_tmp, c(startx, endx), c(x_start_tmp, x_end_tmp))
  data.table::setkeyv(y_tmp, c(by, starty, endy))

  out <- data.table::foverlaps(
    x_tmp,
    y_tmp,
    by.x = c(by, x_start_tmp, x_end_tmp),
    by.y = data.table::key(y_tmp),
    type = "any",
    nomatch = NA
  )

  data.table::setnames(out, c(x_start_tmp, x_end_tmp), c(startx, endx))
  bt_as_tibble(out)
}
