#' Core data manipulation operations
#'
#' Base-faithful table manipulation helpers built around explicit arguments,
#' returning a \code{basetable}. See \code{\link{aggregate}}, \code{\link{count}},
#' \code{\link{drop}}, \code{\link{merge}}, \code{\link{move}},
#' \code{\link{orderrows}}, \code{\link{pick}}, \code{\link{renamecols}},
#' \code{\link{split}}, \code{\link{stack}}, \code{\link{summaries}},
#' \code{\link{uniquerows}}, \code{\link{unstack}}, and \code{\link{within}}
#' for the remaining core verbs, each documented on its own page.
#' @param data A \code{data.frame}.
#' @param subset A base-style expression evaluated against columns.
#' @param select,by Character vectors naming columns. For \code{subset},     \code{by} names optional grouping columns: the \code{subset} expression     is then evaluated within every group, so aggregate references in it     (\code{mean(x)}, \code{max(x)}, ...) are per group; kept rows are     returned in their original order. For \code{transform}, \code{by} names     optional grouping columns and each expression in \code{...} is evaluated     within every group (e.g. \code{cumsum(x)} or \code{x - mean(x)} computed     per group), so the result still has one row per input row. Use     \code{\link{summaries}} for one row per group instead.
#' @param drop,.keep Control flags.
#' @param ... Additional expressions or arguments passed through to helpers.
#' @return Most functions return a basetable.
#' @examples
#' subset(mtcars, cyl == 6, select = c("mpg", "hp"))
#' subset(mtcars, mpg > mean(mpg), by = "cyl")
#' transform(mtcars, power = hp / wt)
#' transform(mtcars, dev_mpg = mpg - mean(mpg), by = "cyl")
#' @export
subset <- function(data, subset = NULL, select = NULL, drop = FALSE, by = NULL) {
  df <- bt_as_data_frame(data)
  penv <- parent.frame()

  subset_expr <- substitute(subset)
  have_subset <- !base::missing(subset) && !identical(subset_expr, quote(NULL))

  select_expr <- substitute(select)
  cols <- if (base::missing(select) || identical(select_expr, quote(NULL))) {
    NULL
  } else {
    col_idx <- as.list(seq_along(df))
    names(col_idx) <- names(df)
    sel <- eval(select_expr, envir = col_idx, enclos = penv)
    if (is.character(sel)) {
      sel <- bt_resolve_cols(df, sel)
    } else if (is.numeric(sel)) {
      sel <- names(df)[sel]
    } else {
      stop("`select` must resolve to column names or positions.", call. = FALSE)
    }
    sel
  }

  if (have_subset && !is.null(by)) {
    # Grouped filter: evaluate the predicate within each group, so aggregate
    # references in it (mean(x), max(x), ...) are per group.
    by <- bt_resolve_cols(df, by)
    grows <- bt_group_rows(bt_engine_groups(df, by)$id)
    keep <- logical(nrow(df))
    for (idx in grows) {
      piece <- df[idx, , drop = FALSE]
      m <- bt_eval_predicate(subset_expr, piece, penv)
      if (!is.logical(m) || length(m) != length(idx)) {
        stop("`subset` must evaluate to a logical vector with one value per row.", call. = FALSE)
      }
      keep[idx] <- m
    }
    out <- bt_engine_subset(df, rows = keep, cols = cols)
    if (drop && ncol(out) == 1L) {
      return(out[[1L]])
    }
    return(out)
  }

  if (have_subset) {
    # Fast route: compiled predicate materialised in one threaded C++ pass,
    # no intermediate logical vector. Falls through when the predicate shape
    # is outside the fused kernel.
    plan <- bt_compile_expr(subset_expr, df)
    if (!is.null(plan)) {
      out <- bt_engine_filter(df, plan, cols)
      if (!is.null(out)) {
        if (drop && ncol(out) == 1L) return(out[[1L]])
        return(out)
      }
    }
    rows <- bt_eval_predicate(subset_expr, df, penv)
    if (!is.logical(rows) || length(rows) != nrow(df)) {
      stop("`subset` must evaluate to a logical vector with one value per row.", call. = FALSE)
    }
  } else {
    rows <- NULL
  }

  out <- bt_engine_subset(df, rows = rows, cols = cols)

  if (drop && ncol(out) == 1L) {
    return(out[[1L]])
  }

  out
}
