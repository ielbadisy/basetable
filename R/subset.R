subset <- function(data, subset = NULL, select = NULL, drop = FALSE) {
  df <- bt_as_data_frame(data)
  data_mask <- as.list(df)

  rows <- rep(TRUE, nrow(df))
  subset_expr <- substitute(subset)
  if (!base::missing(subset) && !identical(subset_expr, quote(NULL))) {
    rows <- eval(subset_expr, envir = data_mask, enclos = parent.frame())
    if (!is.logical(rows) || length(rows) != nrow(df)) {
      stop("`subset` must evaluate to a logical vector with one value per row.", call. = FALSE)
    }
    rows[is.na(rows)] <- FALSE
  }

  out <- df[rows, , drop = FALSE]

  if (!is.null(select)) {
    if (is.character(select)) {
      cols <- bt_resolve_cols(out, select)
      out <- out[, cols, drop = FALSE]
    } else {
      sel <- eval(substitute(select), envir = as.list(seq_along(out)), enclos = as.list(names(out)))
      out <- out[, sel, drop = FALSE]
    }
  }

  if (drop && ncol(out) == 1L) {
    return(out[[1L]])
  }

  bt_as_tibble(out)
}
