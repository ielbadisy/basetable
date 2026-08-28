merge <- function(x,
                  y,
                  by = NULL,
                  all = FALSE,
                  all.x = all,
                  all.y = all,
                  sort = FALSE,
                  suffixes = c(".x", ".y")) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)

  if (is.null(by)) {
    by <- common_names(x_dt, y_dt)
  }
  if (length(by) == 0L) {
    stop("No common join columns found; supply `by` explicitly.", call. = FALSE)
  }
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)

  out <- bt_join_rows(x_dt, y_dt, by = by, all.x = all.x, all.y = all.y, suffixes = suffixes)
  if (sort) out <- bt_engine_order(out, by = by)

  out
}
