subset <- function(data, subset = NULL, select = NULL, drop = FALSE) {
  df <- bt_as_data_frame(data)

  subset_expr <- substitute(subset)
  if (!base::missing(subset) && !identical(subset_expr, quote(NULL))) {
    rows <- bt_eval_predicate(subset_expr, df, parent.frame())
    if (!is.logical(rows) || length(rows) != nrow(df)) {
      stop("`subset` must evaluate to a logical vector with one value per row.", call. = FALSE)
    }
  } else {
    rows <- NULL
  }

  select_expr <- substitute(select)
  cols <- if (base::missing(select) || identical(select_expr, quote(NULL))) {
    NULL
  } else {
    col_idx <- as.list(seq_along(df))
    names(col_idx) <- names(df)
    sel <- eval(select_expr, envir = col_idx, enclos = parent.frame())
    if (is.character(sel)) {
      sel <- bt_resolve_cols(df, sel)
    } else if (is.numeric(sel)) {
      sel <- names(df)[sel]
    } else {
      stop("`select` must resolve to column names or positions.", call. = FALSE)
    }
    sel
  }

  out <- bt_engine_subset(df, rows = rows, cols = cols)

  if (drop && ncol(out) == 1L) {
    return(out[[1L]])
  }

  out
}
