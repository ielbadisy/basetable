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
