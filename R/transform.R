transform <- function(data, ..., .keep = TRUE) {
  df <- bt_as_data_frame(data)
  dots <- as.list(substitute(list(...)))[-1L]

  if (length(dots) == 0L) {
    return(df)
  }

  nms <- names(dots)
  if (is.null(nms) || any(nms == "")) {
    stop("All transformation expressions must be named.", call. = FALSE)
  }

  created <- character()
  env <- list2env(as.list(df), parent = parent.frame())
  for (i in seq_along(dots)) {
    nm <- nms[[i]]
    assign(nm, eval(dots[[i]], envir = env, enclos = parent.frame()), envir = env)
    created <- c(created, nm)
  }

  out <- as.data.frame(as.list.environment(env, all.names = TRUE), stringsAsFactors = FALSE)
  out <- out[, unique(c(names(df), created)), drop = FALSE]

  if (!isTRUE(.keep)) {
    out <- out[, created, drop = FALSE]
  }

  bt_as_data_frame(out)
}

within <- function(data, expr) {
  df <- bt_as_data_frame(data)
  env <- list2env(as.list(df), parent = parent.frame())
  eval(substitute(expr), envir = env)
  out <- as.list.environment(env, all.names = TRUE)
  out <- out[setdiff(names(out), c("expr", "data", "df", "env"))]
  out <- out[vapply(out, function(x) length(x) == nrow(df) || is.null(x), logical(1))]
  out <- out[!vapply(out, is.null, logical(1))]
  bt_as_data_frame(as.data.frame(out, stringsAsFactors = FALSE))
}
