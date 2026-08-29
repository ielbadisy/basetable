aggregate <- function(data, by, value = NULL, fun, ..., na.rm = FALSE, sort = TRUE) {
  if (is.character(data) && length(data) == 1L) {
    return(aggregate_from_file(
      data, by = by, value = value,
      fun = if (missing(fun)) "sum" else fun,
      na.rm = if (missing(na.rm)) TRUE else na.rm, ...
    ))
  }
  df <- bt_as_data_frame(data)
  by <- bt_resolve_cols(df, by)

  if (is.null(value)) {
    value <- setdiff(names(df), by)
  } else {
    value <- bt_resolve_cols(df, value)
  }

  native_fun <- bt_aggregate_fun_name(substitute(fun), fun)
  if (!is.null(native_fun) && length(list(...)) == 0L) {
    out <- bt_engine_aggregate(df, by = by, value = value, fun = native_fun, na.rm = na.rm)
    if (sort && length(by) > 0L) {
      out <- bt_engine_order(out, by = by)
    }
    return(out)
  }

  f <- match.fun(fun)
  f_args <- names(formals(args(f)))
  forward_na_rm <- "na.rm" %in% f_args || "..." %in% f_args

  group_info <- bt_engine_groups(df, by)
  groups <- bt_group_rows(group_info$id)
  keys <- bt_engine_subset(df, rows = group_info$first, cols = by)
  vals <- vector("list", length(groups))
  for (g in seq_along(groups)) {
    idx <- groups[[g]]
    row <- vector("list", length(value))
    names(row) <- value
    for (nm in value) {
      row[[nm]] <- if (forward_na_rm) f(df[[nm]][idx], ..., na.rm = na.rm) else f(df[[nm]][idx], ...)
      if (length(row[[nm]]) != 1L) {
        stop("Aggregate function must return one value per group.", call. = FALSE)
      }
    }
    vals[[g]] <- as.data.frame(row, stringsAsFactors = FALSE)
  }
  out <- bt_as_data_table(cbind(bt_as_data_frame(keys), do.call(rbind, vals)))
  if (sort && length(by) > 0L) {
    out <- bt_engine_order(out, by = by)
  }

  out
}

count <- function(data, by, sort = TRUE, name = "n", ...) {
  if (is.character(data) && length(data) == 1L) {
    return(count_from_file(data, by = by, ...))
  }
  out <- bt_engine_count(data, by = by, name = name)
  if (sort) {
    out <- bt_engine_order(out, by = name, decreasing = TRUE)
  }
  out
}
