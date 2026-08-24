transform <- function(data, ..., .keep = TRUE, by = NULL) {
  dots <- as.list(substitute(list(...)))[-1L]
  bt_transform(data, dots, env = parent.frame(), keep = .keep, by = by)
}

bt_transform <- function(data, dots, env, keep = TRUE, by = NULL) {
  dt <- bt_as_data_table(data)

  if (length(dots) == 0L) {
    return(dt)
  }

  nms <- names(dots)
  if (is.null(nms) || any(nms == "")) {
    stop("All transformation expressions must be named.", call. = FALSE)
  }

  created <- character()
  if (is.null(by)) {
    for (i in seq_along(dots)) {
      nm <- nms[[i]]
      value <- eval(dots[[i]], envir = dt, enclos = env)
      data.table::set(dt, j = nm, value = value)
      created <- c(created, nm)
    }
  } else {
    by_cols <- bt_resolve_cols(dt, by)
    call_env <- new.env(parent = env)
    call_env$dt <- dt
    call_env$by_cols <- by_cols
    for (i in seq_along(dots)) {
      nm <- nms[[i]]
      assign_call <- bquote(dt[, (.(nm)) := .(dots[[i]]), by = by_cols])
      eval(assign_call, envir = call_env)
      created <- c(created, nm)
    }
  }

  if (!isTRUE(keep)) {
    dt <- dt[, unique(created), with = FALSE]
  }

  dt
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
