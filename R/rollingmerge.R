rollingmerge <- function(x, y, by, direction = c("backward", "forward", "nearest"), tolerance = Inf) {
  x_dt <- bt_as_data_table(x)
  y_dt <- bt_as_data_table(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)

  if (length(by) < 1L) {
    stop("`by` must contain at least one column.", call. = FALSE)
  }

  direction <- match.arg(direction)
  roll_key <- by[[length(by)]]
  exact_keys <- if (length(by) > 1L) by[-length(by)] else character(0)

  x_rows <- seq_len(nrow(x_dt))
  match_idx <- rep(NA_integer_, length(x_rows))

  x_groups <- if (length(exact_keys) == 0L) {
    rep(1L, nrow(x_dt))
  } else {
    interaction(x_dt[, exact_keys, drop = FALSE], drop = TRUE, lex.order = TRUE)
  }
  y_groups <- if (length(exact_keys) == 0L) {
    rep(1L, nrow(y_dt))
  } else {
    interaction(y_dt[, exact_keys, drop = FALSE], drop = TRUE, lex.order = TRUE)
  }

  x_split <- base::split(x_rows, x_groups, drop = TRUE)
  y_split <- base::split(seq_len(nrow(y_dt)), y_groups, drop = TRUE)

  x_vals <- x_dt[[roll_key]]
  y_vals_all <- y_dt[[roll_key]]
  tolerance <- as.numeric(tolerance)

  for (group_name in names(x_split)) {
    xr <- x_split[[group_name]]
    yr <- y_split[[group_name]]

    if (length(yr) == 0L) {
      next
    }

    y_ord <- yr[order(y_vals_all[yr], na.last = TRUE)]
    y_vals <- y_vals_all[y_ord]
    x_vals_group <- x_vals[xr]
    group_match <- rep(NA_integer_, length(xr))

    if (direction == "backward") {
      pos <- findInterval(x_vals_group, y_vals, all.inside = FALSE)
      keep <- pos > 0L
      if (is.finite(tolerance)) {
        keep <- keep & (x_vals_group - y_vals[pmax(pos, 1L)] <= tolerance)
      }
      group_match[keep] <- y_ord[pos[keep]]
    } else if (direction == "forward") {
      pos <- findInterval(x_vals_group, y_vals)
      next_pos <- pos + 1L
      exact <- pos > 0L & pos <= length(y_vals) & x_vals_group == y_vals[pmin(pos, length(y_vals))]
      next_pos[exact] <- pos[exact]
      keep <- next_pos >= 1L & next_pos <= length(y_vals)
      if (is.finite(tolerance)) {
        keep <- keep & (y_vals[pmin(next_pos, length(y_vals))] - x_vals_group <= tolerance)
      }
      group_match[keep] <- y_ord[next_pos[keep]]
    } else {
      nearest_pos <- vapply(
        x_vals_group,
        function(value) {
          if (is.na(value)) {
            return(NA_integer_)
          }
          which.min(abs(y_vals - value))
        },
        integer(1L)
      )
      keep <- !is.na(nearest_pos)
      if (is.finite(tolerance)) {
        keep <- keep & (abs(y_vals[nearest_pos] - x_vals_group) <= tolerance)
      }
      group_match[keep] <- y_ord[nearest_pos[keep]]
    }

    match_idx[xr] <- group_match
  }

  out <- data.table::copy(x_dt)
  if (length(by) < ncol(y_dt)) {
    y_extra <- setdiff(names(y_dt), by)
    if (length(y_extra) > 0L) {
      extras <- as.data.frame(y_dt[match_idx, , drop = FALSE], stringsAsFactors = FALSE)[, y_extra, drop = FALSE]
      dup_extra <- intersect(names(extras), names(out))
      if (length(dup_extra) > 0L) {
        names(extras)[match(dup_extra, names(extras))] <- paste0(dup_extra, ".y")
      }
      for (nm in names(extras)) {
        out[[nm]] <- extras[[nm]]
      }
    }
  }

  out
}
