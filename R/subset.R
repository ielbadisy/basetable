subset <- function(data, subset = NULL, select = NULL, drop = FALSE) {
  dt <- bt_as_data_table(data)
  data_mask <- as.list(dt)

  rows <- rep(TRUE, nrow(dt))
  subset_expr <- substitute(subset)
  if (!base::missing(subset) && !identical(subset_expr, quote(NULL))) {
    rows <- eval(subset_expr, envir = data_mask, enclos = parent.frame())
    if (!is.logical(rows) || length(rows) != nrow(dt)) {
      stop("`subset` must evaluate to a logical vector with one value per row.", call. = FALSE)
    }
    rows[is.na(rows)] <- FALSE
  }

  out <- if (is.null(select)) {
    dt[rows]
  } else if (is.character(select)) {
    cols <- bt_resolve_cols(dt, select)
    dt[rows, cols, with = FALSE]
  } else {
    sel <- eval(substitute(select), envir = as.list(seq_along(dt)), enclos = as.list(names(dt)))
    dt[rows, sel, with = FALSE]
  }

  if (drop && ncol(out) == 1L) {
    return(out[[1L]])
  }

  out
}
