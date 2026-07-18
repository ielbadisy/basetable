rollingmerge <- function(x, y, by, direction = c("backward", "forward", "nearest"), tolerance = Inf) {
  x_dt <- bt_as_data_table_ro(x)
  y_dt <- bt_as_data_table_ro(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)

  if (length(by) < 1L) {
    stop("`by` must contain at least one column.", call. = FALSE)
  }

  direction <- match.arg(direction)
  roll_key <- by[[length(by)]]
  roll <- switch(direction, backward = TRUE, forward = -Inf, nearest = "nearest")
  tolerance <- as.numeric(tolerance)

  match_idx <- y_dt[x_dt, on = by, roll = roll, which = TRUE]

  if (is.finite(tolerance)) {
    y_matched <- y_dt[[roll_key]][match_idx]
    bad <- is.na(match_idx) | abs(x_dt[[roll_key]] - y_matched) > tolerance
    match_idx[bad] <- NA_integer_
  }

  out <- data.table::copy(x_dt)
  y_extra <- setdiff(names(y_dt), by)
  if (length(y_extra) > 0L) {
    extras <- data.table::copy(y_dt[match_idx, y_extra, with = FALSE])
    dup_extra <- intersect(names(extras), names(out))
    if (length(dup_extra) > 0L) {
      data.table::setnames(extras, dup_extra, paste0(dup_extra, ".y"))
    }
    out <- cbind(out, extras)
  }

  out
}
