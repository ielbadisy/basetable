subset <- function(data, subset = NULL, select = NULL, drop = FALSE) {
  dt <- bt_as_data_table_ro(data)
  data_mask <- as.list(dt)

  subset_expr <- substitute(subset)
  if (!base::missing(subset) && !identical(subset_expr, quote(NULL))) {
    rows <- eval(subset_expr, envir = data_mask, enclos = parent.frame())
    if (!is.logical(rows) || length(rows) != nrow(dt)) {
      stop("`subset` must evaluate to a logical vector with one value per row.", call. = FALSE)
    }
    rows[is.na(rows)] <- FALSE
  } else {
    rows <- rep(TRUE, nrow(dt))
  }

  select_expr <- substitute(select)
  out <- if (base::missing(select) || identical(select_expr, quote(NULL))) {
    dt[rows]
  } else {
    col_idx <- as.list(seq_along(dt))
    names(col_idx) <- names(dt)
    sel <- eval(select_expr, envir = col_idx, enclos = parent.frame())
    if (is.character(sel)) {
      sel <- bt_resolve_cols(dt, sel)
    }
    dt[rows, sel, with = FALSE]
  }

  if (drop && ncol(out) == 1L) {
    return(out[[1L]])
  }

  out
}
