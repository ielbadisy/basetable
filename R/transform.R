transform <- function(data, ..., .keep = TRUE, by = NULL) {
  dots <- as.list(substitute(list(...)))[-1L]
  bt_transform(data, dots, env = parent.frame(), keep = .keep, by = by)
}

bt_transform <- function(data, dots, env, keep = TRUE, by = NULL) {
  df <- bt_as_data_frame(data)

  if (length(dots) == 0L) {
    return(bt_as_data_table(df))
  }

  nms <- names(dots)
  if (is.null(nms) || any(nms == "")) {
    stop("All transformation expressions must be named.", call. = FALSE)
  }

  created <- character()
  if (is.null(by)) {
    for (i in seq_along(dots)) {
      nm <- nms[[i]]
      value <- eval(dots[[i]], envir = bt_data_mask(df, env), enclos = env)
      df[[nm]] <- bt_set_row_names(value, nrow(df))
      created <- c(created, nm)
    }
  } else {
    by_cols <- bt_resolve_cols(df, by)
    groups <- bt_group_rows(bt_engine_groups(df, by_cols)$id)
    for (i in seq_along(dots)) {
      nm <- nms[[i]]
      out <- vector("list", length(groups))
      for (g in seq_along(groups)) {
        idx <- groups[[g]]
        mask <- bt_data_mask(df[idx, , drop = FALSE], env)
        value <- eval(dots[[i]], envir = mask, enclos = env)
        out[[g]] <- bt_set_row_names(value, length(idx))
      }
      value <- vector(typeof(out[[1L]]), nrow(df))
      for (g in seq_along(groups)) {
        value[groups[[g]]] <- out[[g]]
      }
      df[[nm]] <- value
      created <- c(created, nm)
    }
  }

  if (!isTRUE(keep)) {
    df <- df[, unique(created), drop = FALSE]
  }

  bt_as_data_table(df)
}

within <- function(data, expr) {
  df <- bt_as_data_frame(data)
  env <- list2env(as.list(df), parent = parent.frame())
  eval(substitute(expr), envir = env)
  out <- as.list.environment(env, all.names = TRUE)
  out <- out[setdiff(names(out), c("expr", "data", "df", "env"))]
  out <- out[vapply(out, function(x) length(x) == nrow(df) || is.null(x), logical(1))]
  out <- out[!vapply(out, is.null, logical(1))]
  bt_as_data_table(as.data.frame(out, stringsAsFactors = FALSE))
}
