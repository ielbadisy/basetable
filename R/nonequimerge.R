#' Merge two tables using non-equi (inequality) conditions
#'
#' Unlike a plain equi-\code{\link{merge}}, \code{nonequimerge()} supports
#' inequality conditions between columns of \code{x} and \code{y} (for example,
#' matching each \code{x} row to every \code{y} row whose date range contains
#' \code{x}'s date), in addition to any exact-match columns in \code{by}.
#' @param x,y Data frames or data tables to join.
#' @param by Character vector of join conditions. A plain column name (present in both \code{x} and \code{y}) is matched exactly; an entry containing a comparison operator, written as \code{"xcol<op>ycol"} (e.g. \code{"date>=start_date"}, \code{"date<=end_date"}), is matched as an inequality between \code{x}'s \code{xcol} and \code{y}'s \code{ycol}. This uses \code{"col>=other"} style comparison strings.
#' @param ... Currently ignored; reserved for future join (e.g. \code{mult}, \code{roll}).
#' @return A basetable with one row per matching \code{x}/\code{y} pair (an inner
#' join: rows of \code{x} with no matching \code{y} row are dropped).
#' @examples
#' x <- data.frame(id = 1, date = as.Date("2024-01-15"))
#' y <- data.frame(
#'   id = 1,
#'   start_date = as.Date(c("2024-01-01", "2024-02-01")),
#'   end_date = as.Date(c("2024-01-31", "2024-02-28")),
#'   period = c("Jan", "Feb")
#' )
#' nonequimerge(x, y, by = c("id", "date>=start_date", "date<=end_date"))
#' @export
nonequimerge <- function(x, y, by, ...) {
  x_df <- bt_as_data_frame(x)
  y_df <- bt_as_data_frame(y)

  if (length(by) < 1L) {
    stop("`by` must contain at least one column.", call. = FALSE)
  }

  is_condition <- grepl("[<>=!]", by)
  plain <- by[!is_condition]
  if (length(plain) > 0L) {
    bt_resolve_cols(x_df, plain)
    bt_resolve_cols(y_df, plain)
  }
  if (!any(is_condition)) {
    return(bt_join_rows(x_df, y_df, by = plain))
  }

  cond <- by[is_condition]
  predicates <- lapply(cond, function(z) {
    m <- regexec("^\\s*([^<>=!]+)\\s*(<=|>=|==|<|>)\\s*([^<>=!]+)\\s*$", z)
    p <- regmatches(z, m)[[1L]]
    if (length(p) != 4L) stop("Invalid non-equi condition.", call. = FALSE)
    list(x = trimws(p[[2L]]), op = p[[3L]], y = trimws(p[[4L]]))
  })

  y_cols <- setdiff(names(y_df), plain)
  bt_range_join(
    x_df, y_df,
    by = plain,
    predicates = predicates,
    y_cols = y_cols,
    all.x = FALSE
  )
}
