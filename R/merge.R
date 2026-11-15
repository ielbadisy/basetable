#' Merge two tables
#'
#' Merge two tables using shared keys.
#' @param x,y Data frames or data tables to merge.
#' @param by Optional character vector of join columns.
#' @param all,all.x,all.y Controls for keeping unmatched rows.
#' @param sort Whether to sort the result.
#' @param suffixes Suffixes used for overlapping column names.
#' @param ... Arguments of [base::merge()] only (`by.x`, `by.y`,
#'   `incomparables`, `no.dups`, ...). When any is given, the whole call is
#'   passed to [base::merge()] and its result is returned.
#' @details basetable's `merge()` masks [base::merge()] when the package is
#'   attached. Calls that use only the arguments above keep basetable's
#'   behaviour (`sort = FALSE` by default, a basetable result). Calls with
#'   base-only arguments go to [base::merge()], so base code keeps working.
#' @return A basetable containing the merged rows.
#' @examples
#' a <- data.frame(id = 1:3, u = c("x", "y", "z"))
#' b <- data.frame(id = c(2, 3, 4), v = c(10, 20, 30))
#' merge(a, b, by = "id")
#' merge(a, b, by.x = "id", by.y = "id", all = TRUE) # base::merge()
#' @export
merge <- function(x,
                  y,
                  by = NULL,
                  all = FALSE,
                  all.x = all,
                  all.y = all,
                  sort = FALSE,
                  suffixes = c(".x", ".y"),
                  ...) {
  if (...length() > 0L) {
    return(bt_call_base(sys.call(), quote(base::merge), parent.frame()))
  }
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)

  if (is.null(by)) {
    by <- commonnames(x_dt, y_dt)
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
