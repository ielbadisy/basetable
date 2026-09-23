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
  # Factor keys match by label; like base::merge(), keep the key a factor
  # with x's levels, adding y's new levels only when y rows are kept.
  for (k in by) {
    if (is.factor(x_dt[[k]]) && is.factor(y_dt[[k]]) && !is.factor(out[[k]])) {
      lev <- levels(x_dt[[k]])
      if (isTRUE(all.y)) lev <- union(lev, levels(y_dt[[k]]))
      out[[k]] <- factor(out[[k]], levels = lev)
    }
  }
  if (sort) out <- bt_engine_order(out, by = by)

  out
}
