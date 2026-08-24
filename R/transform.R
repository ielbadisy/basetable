transform <- function(data, ..., .keep = TRUE) {
  dots <- as.list(substitute(list(...)))[-1L]
  bt_transform(data, dots, env = parent.frame(), keep = .keep)
}

bt_transform <- function(data, dots, env, keep = TRUE) {
  dt <- bt_as_data_table(data)

  if (length(dots) == 0L) {
    return(dt)
  }

  nms <- names(dots)
  if (is.null(nms) || any(nms == "")) {
    stop("All transformation expressions must be named.", call. = FALSE)
  }

  created <- character()
  for (i in seq_along(dots)) {
    nm <- nms[[i]]
    value <- eval(dots[[i]], envir = dt, enclos = env)
    data.table::set(dt, j = nm, value = value)
    created <- c(created, nm)
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
