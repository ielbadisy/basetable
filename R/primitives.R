`%||%` <- function(x, y) if (is.null(x)) y else x

nrows <- function(data) nrow(bt_as_data_frame(data))

ncols <- function(data) ncol(bt_as_data_frame(data))

colnames <- function(data) names(bt_as_data_frame(data))

rownames <- function(data) base::rownames(bt_as_data_frame(data))

classes <- function(data) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(data.frame(column = names(df), class = vapply(df, bt_mode, character(1)), stringsAsFactors = FALSE))
}

uniques <- function(data) {
  df <- bt_as_data_frame(data)
  stats::setNames(vapply(df, bt_distinct_n, integer(1)), names(df))
}

cardinality <- function(data, cols, prop = FALSE) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  out <- vapply(cols, function(nm) {
    n <- bt_distinct_n(df[[nm]])
    if (prop) n / nrow(df) else n
  }, numeric(1))
  stats::setNames(out, cols)
}

constants <- function(data) {
  df <- bt_as_data_frame(data)
  keep <- vapply(df, function(x) length(unique(stats::na.omit(x))) == 1L, logical(1))
  names(df)[keep]
}

emptycols <- function(data) {
  df <- bt_as_data_frame(data)
  names(df)[vapply(df, bt_is_blank, logical(1)) | vapply(df, function(x) all(is.na(x)), logical(1))]
}

emptyrows <- function(data) {
  df <- bt_as_data_frame(data)
  keep <- apply(df, 1L, function(x) all(bt_is_blank(x)))
  df[keep, , drop = FALSE]
}

duplicaterows <- function(data) {
  df <- bt_as_data_frame(data)
  df[duplicated(df) | duplicated(df, fromLast = TRUE), , drop = FALSE]
}

duplicatekeys <- function(data, by) {
  duplicated_keys(data, by)
}

duplicatenames <- function(data) {
  df <- bt_as_data_frame(data)
  names(df)[duplicated(names(df))]
}

commonnames <- function(x, y) {
  common_names(x, y)
}

cleannames <- function(data) {
  df <- bt_as_data_frame(data)
  names(df) <- bt_clean_names(names(df), method = "unique")
  bt_as_tibble(df)
}

repairnames <- function(data, method = c("unique", "universal", "minimal")) {
  df <- bt_as_data_frame(data)
  names(df) <- bt_clean_names(names(df), method = match.arg(method))
  bt_as_tibble(df)
}

pick <- function(data, cols) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  bt_as_tibble(df[, cols, drop = FALSE])
}

drop <- function(data, cols) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  keep <- setdiff(names(df), cols)
  bt_as_tibble(df[, keep, drop = FALSE])
}

rename <- function(data, old = NULL, new = NULL, ...) {
  df <- bt_as_data_frame(data)
  dots <- as.list(substitute(list(...)))[-1L]

  if (!is.null(old) || !is.null(new)) {
    old <- bt_resolve_cols(df, old)
    if (is.null(new) || length(new) != length(old)) {
      stop("`old` and `new` must have the same length.", call. = FALSE)
    }
    names(df)[match(old, names(df))] <- new
    return(bt_as_tibble(df))
  }

  if (length(dots) == 0L) {
    return(bt_as_tibble(df))
  }
  new_names <- names(dots)
  if (is.null(new_names) || any(!nzchar(new_names))) {
    stop("Rename expressions must be named.", call. = FALSE)
  }
  old_names <- vapply(dots, bt_rename_old_name, character(1), enclos = parent.frame())
  old_names <- bt_resolve_cols(df, old_names)
  names(df)[match(old_names, names(df))] <- new_names
  bt_as_tibble(df)
}

renamewith <- function(data, cols, fun) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  names(df)[match(cols, names(df))] <- vapply(cols, function(x) fun(x), character(1))
  bt_as_tibble(df)
}

move <- function(data, cols, before = NULL, after = NULL) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  remaining <- setdiff(names(df), cols)
  if (!is.null(before) && !is.null(after)) {
    stop("Use only one of `before` or `after`.", call. = FALSE)
  }
  if (is.null(before) && is.null(after)) {
    order <- c(cols, remaining)
  } else if (!is.null(before)) {
    if (is.numeric(before)) {
      pos <- before
    } else {
      before <- bt_resolve_cols(df, before)
      if (length(before) != 1L) stop("`before` must identify one column.", call. = FALSE)
      pos <- match(before, remaining)
    }
    order <- append(remaining, cols, after = pos - 1L)
  } else {
    if (is.numeric(after)) {
      pos <- after
    } else {
      after <- bt_resolve_cols(df, after)
      if (length(after) != 1L) stop("`after` must identify one column.", call. = FALSE)
      pos <- match(after, remaining)
    }
    order <- append(remaining, cols, after = pos)
  }
  bt_as_tibble(df[, order, drop = FALSE])
}

firstcols <- function(data, cols) move(data, cols, before = 1L)

lastcols <- function(data, cols) move(data, cols, after = ncol(bt_as_data_frame(data)) - length(bt_resolve_cols(bt_as_data_frame(data), cols)))

slice <- function(data, rows) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(df[rows, , drop = FALSE])
}

firstrows <- function(data, n = 1L) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(utils::head(df, n))
}

lastrows <- function(data, n = 1L) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(utils::tail(df, n))
}

samplerows <- function(data, n) {
  df <- bt_as_data_frame(data)
  idx <- sample.int(nrow(df), n)
  bt_as_tibble(df[idx, , drop = FALSE])
}

samplefrac <- function(data, frac) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(df[sample.int(nrow(df), ceiling(nrow(df) * frac)), , drop = FALSE])
}

orderrows <- function(data, by, decreasing = FALSE, na.last = TRUE) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(bt_order_data(df, by, decreasing = decreasing, na.last = na.last))
}

reverse <- function(data) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(df[rev(seq_len(nrow(df))), , drop = FALSE])
}

distinct <- function(data, by = NULL, cols = NULL, .keep_all = TRUE) {
  df <- bt_as_data_frame(data)
  by <- by %||% cols
  if (is.null(by)) {
    return(bt_as_tibble(unique(df)))
  }
  by <- bt_resolve_cols(df, by)
  keep <- !duplicated(df[, by, drop = FALSE])
  out <- if (.keep_all) df[keep, , drop = FALSE] else df[keep, by, drop = FALSE]
  bt_as_tibble(out)
}

firstby <- function(data, by, order = NULL) {
  df <- bt_as_data_frame(data)
  if (!is.null(order)) df <- bt_order_data(df, order)
  df <- df[!duplicated(df[, bt_resolve_cols(df, by), drop = FALSE]), , drop = FALSE]
  bt_as_tibble(df)
}

lastby <- function(data, by, order = NULL) {
  df <- bt_as_data_frame(data)
  if (!is.null(order)) df <- bt_order_data(df, order)
  key <- bt_resolve_cols(df, by)
  keep <- !duplicated(df[, key, drop = FALSE], fromLast = TRUE)
  bt_as_tibble(df[keep, , drop = FALSE])
}

removeduplicates <- function(data, by = NULL, keep = c("first", "last", "none")) {
  keep <- match.arg(keep)
  if (is.null(by)) {
    df <- bt_as_data_frame(data)
    return(bt_as_tibble(if (keep == "first") df[!duplicated(df), , drop = FALSE] else if (keep == "last") df[!duplicated(df, fromLast = TRUE), , drop = FALSE] else df[FALSE, , drop = FALSE]))
  }
  if (keep == "first") return(distinct(data, by = by, .keep_all = TRUE))
  if (keep == "last") return(lastby(data, by = by))
  df <- bt_as_data_frame(data)
  key <- bt_resolve_cols(df, by)
  bt_as_tibble(df[!duplicated(df[, key, drop = FALSE]) & !duplicated(df[, key, drop = FALSE], fromLast = TRUE), , drop = FALSE])
}

rowmin <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  apply(df, 1L, min, na.rm = na.rm)
}

rowmax <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  apply(df, 1L, max, na.rm = na.rm)
}

rowany <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  apply(df, 1L, function(x) any(x, na.rm = na.rm))
}

rowall <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  apply(df, 1L, function(x) all(x, na.rm = na.rm))
}

rowcount <- function(data, cols = NULL, value = TRUE, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  apply(df, 1L, function(x) sum(x == value, na.rm = na.rm))
}

rowfirst <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  apply(df, 1L, function(x) {
    x <- x[!is.na(x) | !na.rm]
    x[[1L]]
  })
}

rowlast <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  apply(df, 1L, function(x) {
    x <- x[!is.na(x) | !na.rm]
    tail(x, 1L)
  })
}

rowapply <- function(data, cols = NULL, fun, ...) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  apply(df, 1L, fun, ...)
}

lagvalue <- function(x, n = 1L, default = NA) {
  c(rep(default, n), head(x, -n))
}

leadvalue <- function(x, n = 1L, default = NA) {
  c(tail(x, -n), rep(default, n))
}

difference <- function(x, lag = 1L) {
  c(rep(NA, lag), diff(x, lag = lag))
}

rownumber <- function(x) seq_along(x)

denserank <- function(x) match(x, unique(x))

percentrank <- function(x) stats::rank(x, ties.method = "average", na.last = "keep") / sum(!is.na(x))

cumedist <- function(x) cumsum(!duplicated(x)) / seq_along(x)

cummean <- function(x) cumsum(x) / seq_along(x)

cumcount <- function(x) seq_along(x)

bt_roll_window <- function(n, width, align) {
  switch(
    align,
    right = lapply(seq_len(n), function(i) seq.int(max(1L, i - width + 1L), i)),
    left = lapply(seq_len(n), function(i) seq.int(i, min(n, i + width - 1L))),
    center = {
      left <- floor((width - 1L) / 2L)
      right <- width - left - 1L
      lapply(seq_len(n), function(i) seq.int(max(1L, i - left), min(n, i + right)))
    }
  )
}

bt_roll_apply <- function(x, width, FUN, align = c("right", "left", "center"), fill = NA, ...) {
  align <- match.arg(align)
  idxs <- bt_roll_window(length(x), width, align)
  out <- vapply(idxs, function(idx) {
    if (length(idx) == 0L) return(fill)
    FUN(x[idx], ...)
  }, FUN.VALUE = FUN(x[seq_len(min(width, length(x)))], ...))
  out
}

rollmean <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollmean", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollmean(x, n = width, align = match.arg(align), fill = fill))
  bt_roll_apply(x, width, mean, align = align, fill = fill, na.rm = na.rm)
}

rollsum <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollsum", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollsum(x, n = width, align = match.arg(align), fill = fill))
  bt_roll_apply(x, width, sum, align = align, fill = fill, na.rm = na.rm)
}

rollmin <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollmin", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollmin(x, n = width, align = match.arg(align), fill = fill))
  bt_roll_apply(x, width, min, align = align, fill = fill, na.rm = na.rm)
}

rollmax <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollmax", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollmax(x, n = width, align = match.arg(align), fill = fill))
  bt_roll_apply(x, width, max, align = align, fill = fill, na.rm = na.rm)
}

rollmedian <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollmedian", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollmedian(x, n = width, align = match.arg(align), fill = fill))
  bt_roll_apply(x, width, stats::median, align = align, fill = fill, na.rm = na.rm)
}

rollsd <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollsd", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollsd(x, n = width, align = match.arg(align), fill = fill))
  bt_roll_apply(x, width, stats::sd, align = align, fill = fill, na.rm = na.rm)
}

rollvar <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollapply", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollapply(x, n = width, FUN = stats::var, align = match.arg(align), fill = fill))
  bt_roll_apply(x, width, stats::var, align = align, fill = fill, na.rm = na.rm)
}

rollprod <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollprod", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollprod(x, n = width, align = match.arg(align), fill = fill))
  bt_roll_apply(x, width, prod, align = align, fill = fill, na.rm = na.rm)
}

rollapply <- function(x, width, FUN, ..., align = c("right", "left", "center"), fill = NA, partial = FALSE) {
  if (exists("frollapply", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollapply(x, n = width, FUN = FUN, ..., align = match.arg(align), fill = fill))
  bt_roll_apply(x, width, FUN, align = align, fill = fill, ...)
}

applycols <- function(data, cols, fun, ...) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  for (nm in cols) df[[nm]] <- fun(df[[nm]], ...)
  bt_as_tibble(df)
}

replacecols <- function(data, cols, values) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  if (length(values) == 1L && is.list(values[[1L]]) && !is.data.frame(values[[1L]])) values <- values[[1L]]
  if (length(values) != length(cols)) stop("`values` must match `cols`.", call. = FALSE)
  for (i in seq_along(cols)) df[[cols[[i]]]] <- values[[i]]
  bt_as_tibble(df)
}

replacewhere <- function(data, condition, cols, value) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  cond <- bt_eval_logical(substitute(condition), df, nrow(df))
  for (nm in cols) {
    vec <- df[[nm]]
    vec[cond] <- value
    df[[nm]] <- vec
  }
  bt_as_tibble(df)
}

transform <- function(data, ..., by = NULL, keep = TRUE, .keep = keep) {
  df <- bt_as_data_frame(data)
  dots <- as.list(substitute(list(...)))[-1L]
  if (length(dots) == 0L) return(bt_as_tibble(df))
  nms <- names(dots)
  if (is.null(nms) || any(nms == "")) stop("All transformation expressions must be named.", call. = FALSE)
  if (is.null(by)) {
    env <- list2env(as.list(df), parent = parent.frame())
    for (i in seq_along(dots)) assign(nms[[i]], eval(dots[[i]], env, parent.frame()), envir = env)
    out <- as.data.frame(as.list.environment(env, all.names = TRUE), stringsAsFactors = FALSE)
    out <- out[, unique(c(names(df), nms)), drop = FALSE]
    if (!isTRUE(.keep)) out <- out[, nms, drop = FALSE]
    return(bt_as_tibble(out))
  }
  pieces <- bt_split_by(df, by = by, drop = FALSE, keepby = TRUE)
  out <- lapply(pieces, function(piece) {
    env <- list2env(as.list(piece), parent = parent.frame())
    for (i in seq_along(dots)) assign(nms[[i]], eval(dots[[i]], env, parent.frame()), envir = env)
    tmp <- as.data.frame(as.list.environment(env, all.names = TRUE), stringsAsFactors = FALSE)
    tmp <- tmp[, unique(c(names(piece), nms)), drop = FALSE]
    if (!isTRUE(.keep)) tmp <- tmp[, c(by, nms), drop = FALSE]
    tmp
  })
  bt_as_tibble(data.table::rbindlist(out, fill = TRUE))
}

within <- function(data, expr, by = NULL) {
  df <- bt_as_data_frame(data)
  expr <- substitute(expr)
  if (is.null(by)) {
    env <- list2env(as.list(df), parent = parent.frame())
    eval(expr, envir = env)
    out <- as.list.environment(env, all.names = TRUE)
    out <- out[vapply(out, function(x) length(x) == nrow(df) || is.null(x), logical(1))]
    out <- out[!vapply(out, is.null, logical(1))]
    return(bt_as_tibble(as.data.frame(out, stringsAsFactors = FALSE)))
  }
  pieces <- bt_split_by(df, by = by, keepby = TRUE)
  out <- lapply(pieces, function(piece) {
    env <- list2env(as.list(piece), parent = parent.frame())
    eval(expr, envir = env)
    tmp <- as.list.environment(env, all.names = TRUE)
    tmp <- tmp[vapply(tmp, function(x) length(x) == nrow(piece) || is.null(x), logical(1))]
    tmp <- tmp[!vapply(tmp, is.null, logical(1))]
    bt_as_tibble(as.data.frame(tmp, stringsAsFactors = FALSE))
  })
  bt_as_tibble(data.table::rbindlist(out, fill = TRUE))
}

summaries <- function(data, by = NULL, ...) {
  df <- bt_as_data_frame(data)
  dots <- as.list(substitute(list(...)))[-1L]
  if (length(dots) == 0L) stop("At least one summary expression is required.", call. = FALSE)
  nms <- names(dots)
  if (is.null(nms) || any(nms == "")) stop("All summary expressions must be named.", call. = FALSE)
  pieces <- if (is.null(by)) list(df) else bt_split_by(df, by = by, keepby = TRUE)
  rows <- lapply(pieces, function(piece) {
    env <- list2env(as.list(piece), parent = parent.frame())
    vals <- lapply(dots, function(expr) eval(expr, env, parent.frame()))
    if (any(vapply(vals, length, integer(1)) != 1L)) stop("Each summary expression must return one value.", call. = FALSE)
    out <- as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
    names(out) <- nms
    if (!is.null(by)) out <- cbind(piece[1L, bt_resolve_cols(piece, by), drop = FALSE], out, stringsAsFactors = FALSE)
    out
  })
  bt_as_tibble(data.table::rbindlist(rows, fill = TRUE))
}

propcount <- function(data, by, margin = NULL) {
  out <- count(data, by = by, sort = FALSE, name = "n")
  if (is.null(margin)) {
    out$prop <- out$n / sum(out$n)
    return(out)
  }
  margin <- bt_resolve_cols(bt_as_data_frame(out), margin)
  total <- stats::aggregate(out$n, out[margin], sum)
  names(total)[ncol(total)] <- ".total"
  out <- merge(out, total, by = margin, sort = FALSE)
  out$prop <- out$n / out$.total
  out$.total <- NULL
  out
}

applyby <- function(data, by, fun, ..., bind = FALSE, id = ".group") {
  pieces <- bt_split_by(data, by = by, keepby = TRUE)
  out <- lapply(pieces, function(piece) fun(piece, ...))
  if (!bind) return(out)
  recombine(out, id = id)
}

recombine <- function(x, id = NULL) {
  combine(x, id = id)
}

count <- function(data, by, sort = TRUE, name = "n") {
  df <- bt_as_data_frame(data)
  by <- bt_resolve_cols(df, by)
  out <- stats::aggregate(rep(1L, nrow(df)), df[, by, drop = FALSE], sum)
  names(out)[ncol(out)] <- name
  if (sort) out <- out[order(out[[name]], decreasing = TRUE), , drop = FALSE]
  bt_as_tibble(out)
}

split <- function(data, by, drop = FALSE, keep.by = FALSE, keepby = keep.by) {
  bt_split_by(data, by = by, drop = drop, keepby = keepby)
}

aggregate <- function(data, by, value = NULL, fun, ..., na.rm = FALSE, sort = TRUE) {
  df <- bt_as_data_frame(data)
  by <- bt_resolve_cols(df, by)
  if (is.null(value)) value <- setdiff(names(df), by) else value <- bt_resolve_cols(df, value)
  pieces <- split(df[, c(by, value), drop = FALSE], by = by, keep.by = TRUE)
  f <- match.fun(fun)
  rows <- lapply(pieces, function(piece) {
    vals <- lapply(piece[, value, drop = FALSE], function(x) f(x, ..., na.rm = na.rm))
    out <- as.data.frame(vals, stringsAsFactors = FALSE)
    names(out) <- value
    cbind(piece[1L, by, drop = FALSE], out, stringsAsFactors = FALSE)
  })
  out <- data.table::rbindlist(rows, fill = TRUE)
  if (sort) out <- out[do.call(order, out[by])]
  bt_as_tibble(out)
}

unmatchedkeys <- function(x, y, by) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  key_y <- unique(y_dt[, by, drop = FALSE])
  keep <- !duplicated(x_dt[, by, drop = FALSE]) & is.na(match(paste(x_dt[, by, drop = FALSE]), paste(key_y[, by, drop = FALSE])))
  bt_as_tibble(x_dt[keep, , drop = FALSE])
}

matchedkeys <- function(x, y, by) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  key_x <- interaction(x_dt[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  key_y <- interaction(y_dt[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  keep <- key_x %in% key_y
  bt_as_tibble(x_dt[keep, , drop = FALSE])
}

joinrelationship <- function(x, y, by) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  x_dup <- any(duplicated(x_dt[, by, drop = FALSE]))
  y_dup <- any(duplicated(y_dt[, by, drop = FALSE]))
  if (!x_dup && !y_dup) "one-to-one" else if (!x_dup && y_dup) "one-to-many" else if (x_dup && !y_dup) "many-to-one" else "many-to-many"
}

semimerge <- function(x, y, by) matchedkeys(x, y, by)

antimerge <- function(x, y, by) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  key_x <- interaction(x_dt[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  key_y <- interaction(y_dt[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  keep <- !(key_x %in% key_y)
  bt_as_tibble(x_dt[keep, , drop = FALSE])
}

crossmerge <- function(x, y) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)
  out <- merge(x_dt, y_dt, by = NULL, all = TRUE, sort = FALSE)
  bt_as_tibble(out)
}

updatemerge <- function(x, y, by, cols = NULL) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  if (is.null(cols)) cols <- setdiff(intersect(names(x_dt), names(y_dt)), by)
  cols <- bt_resolve_cols(x_dt, cols)
  key <- merge(x_dt[, by, drop = FALSE], y_dt[, c(by, cols), drop = FALSE], by = by, all.x = TRUE, sort = FALSE, suffixes = c("", ".y"))
  for (nm in cols) {
    y_nm <- paste0(nm, ".y")
    if (y_nm %in% names(key)) {
      x_dt[[nm]] <- ifelse(is.na(key[[y_nm]]), x_dt[[nm]], key[[y_nm]])
    }
  }
  bt_as_tibble(x_dt)
}

rollingmerge <- function(x, y, by, direction = c("backward", "forward", "nearest"), tolerance = Inf) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  direction <- match.arg(direction)
  if (length(by) < 1L) stop("`by` is required.", call. = FALSE)
  idx <- by[[length(by)]]
  keys <- by[length(by)]
  x_dt <- x_dt[order(x_dt[[idx]]), , drop = FALSE]
  y_dt <- y_dt[order(y_dt[[idx]]), , drop = FALSE]
  out <- merge(x_dt, y_dt, by = by, all.x = TRUE, sort = FALSE, suffixes = c(".x", ".y"))
  bt_as_tibble(out)
}

nearestmerge <- function(x, y, by, tolerance = Inf) {
  rollingmerge(x, y, by = by, direction = "nearest", tolerance = tolerance)
}

rangemerge <- function(x, y, by, lower, upper) {
  x_dt <- bt_as_data_frame(x); y_dt <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_dt, by); bt_resolve_cols(y_dt, by)
  lower <- bt_resolve_cols(x_dt, lower); upper <- bt_resolve_cols(x_dt, upper)
  bt_as_tibble(merge(x_dt, y_dt, by = by, all.x = TRUE, sort = FALSE))
}

overlapmerge <- function(x, y, startx, endx, starty, endy, by = NULL) {
  bt_as_tibble(merge(bt_as_data_frame(x), bt_as_data_frame(y), by = by, all = FALSE, sort = FALSE))
}

nonequimerge <- function(x, y, by, ...) {
  bt_as_tibble(merge(bt_as_data_frame(x), bt_as_data_frame(y), by = by, all = FALSE, sort = FALSE, ...))
}

rbindfill <- function(..., id = NULL, fill = TRUE, typeconflict = c("error", "coerce")) {
  typeconflict <- match.arg(typeconflict)
  dots <- list(...)
  if (length(dots) == 1L && is.list(dots[[1L]]) && !inherits(dots[[1L]], "data.frame")) dots <- dots[[1L]]
  bt_as_tibble(data.table::rbindlist(lapply(dots, bt_as_data_frame), fill = fill, idcol = id))
}

unionrows <- function(x, y, by = NULL) {
  df <- rbindfill(x, y)
  if (!is.null(by)) df <- distinct(df, by = by, .keep_all = TRUE)
  bt_as_tibble(unique(bt_as_data_frame(df)))
}

intersectrows <- function(x, y, by = NULL) {
  x_df <- bt_as_data_frame(x); y_df <- bt_as_data_frame(y)
  if (is.null(by)) return(bt_as_tibble(intersect(x_df, y_df)))
  by <- bt_resolve_cols(x_df, by); bt_resolve_cols(y_df, by)
  key_y <- unique(y_df[, by, drop = FALSE])
  keep <- !is.na(match(paste(x_df[, by, drop = FALSE]), paste(key_y[, by, drop = FALSE])))
  bt_as_tibble(x_df[keep, , drop = FALSE])
}

diffrows <- function(x, y, by = NULL) {
  x_df <- bt_as_data_frame(x); y_df <- bt_as_data_frame(y)
  if (is.null(by)) return(bt_as_tibble(setdiff(x_df, y_df)))
  by <- bt_resolve_cols(x_df, by); bt_resolve_cols(y_df, by)
  key_y <- unique(y_df[, by, drop = FALSE])
  keep <- is.na(match(paste(x_df[, by, drop = FALSE]), paste(key_y[, by, drop = FALSE])))
  bt_as_tibble(x_df[keep, , drop = FALSE])
}

equalrows <- function(x, y, by = NULL) {
  x_df <- bt_as_data_frame(x); y_df <- bt_as_data_frame(y)
  if (is.null(by)) return(identical(x_df, y_df))
  by <- bt_resolve_cols(x_df, by); bt_resolve_cols(y_df, by)
  identical(sort(paste(x_df[, by, drop = FALSE])), sort(paste(y_df[, by, drop = FALSE])))
}

tolong <- function(data, cols, names = "variable", values = "value", idcols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  idcols <- if (is.null(idcols)) setdiff(names(df), cols) else bt_resolve_cols(df, idcols)
  bt_as_tibble(data.table::melt(data.table::as.data.table(df), id.vars = idcols, measure.vars = cols, variable.name = names, value.name = values, na.rm = na.rm))
}

towide <- function(data, names, values, idcols = NULL, fun = NULL, fill = NA) {
  df <- bt_as_data_frame(data)
  idcols <- if (is.null(idcols)) setdiff(names(df), c(names, values)) else bt_resolve_cols(df, idcols)
  bt_as_tibble(data.table::dcast(data.table::as.data.table(df), formula = stats::reformulate(c(idcols, names), response = values), fun.aggregate = fun %||% length, fill = fill))
}

separate <- function(data, column, into, sep, remove = TRUE, extra = c("warn", "drop", "merge"), fill = c("warn", "left", "right")) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  extra <- match.arg(extra); fill <- match.arg(fill)
  parts <- strsplit(as.character(df[[column]]), sep, fixed = FALSE)
  max_len <- length(into)
  out <- do.call(rbind, lapply(parts, function(x) {
    x <- x[seq_len(min(length(x), max_len))]
    if (length(x) < max_len) x <- c(x, rep(NA_character_, max_len - length(x)))
    as.data.frame(as.list(x), stringsAsFactors = FALSE)
  }))
  names(out) <- into
  if (remove) df[[column]] <- NULL
  bt_as_tibble(cbind(df, out, stringsAsFactors = FALSE))
}

unite <- function(data, column, cols, sep = "_", remove = TRUE, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  new <- apply(df[, cols, drop = FALSE], 1L, function(x) {
    if (na.rm) x <- x[!is.na(x)]
    paste(x, collapse = sep)
  })
  keep <- if (remove) setdiff(names(df), cols) else names(df)
  df <- df[, keep, drop = FALSE]
  df[[column]] <- new
  bt_as_tibble(df)
}

expandrows <- function(data, times) {
  df <- bt_as_data_frame(data)
  times <- bt_set_row_names(times, nrow(df))
  bt_as_tibble(df[rep(seq_len(nrow(df)), times), , drop = FALSE])
}

completegrid <- function(data, cols, fill = list()) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  grid <- expand.grid(lapply(df[cols], function(x) unique(x)), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  out <- merge(grid, df, by = cols, all.x = TRUE, sort = FALSE)
  for (nm in names(fill)) if (nm %in% names(out)) out[[nm]][is.na(out[[nm]])] <- fill[[nm]]
  bt_as_tibble(out)
}

transpose <- function(data) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(as.data.frame(t(df), stringsAsFactors = FALSE))
}

naif <- function(x, values) {
  x[x %in% values] <- NA
  x
}

nato <- function(x, value) {
  x[is.na(x)] <- value
  x
}

blanktona <- function(x) {
  x[bt_is_blank(x)] <- NA
  x
}

natoblank <- function(x) {
  x[is.na(x)] <- ""
  x
}

replacevalues <- function(x, old, new) {
  map <- stats::setNames(new, old)
  out <- x
  idx <- match(as.character(x), names(map))
  keep <- !is.na(idx)
  out[keep] <- unname(map[idx[keep]])
  out
}

missingrows <- function(data, cols = NULL, mode = c("any", "all")) {
  df <- bt_as_data_frame(data)
  mode <- match.arg(mode)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  keep <- if (mode == "any") apply(df, 1L, function(x) any(bt_is_blank(x))) else apply(df, 1L, function(x) all(bt_is_blank(x)))
  bt_as_tibble(df[keep, , drop = FALSE])
}

omitmissing <- function(data, cols = NULL, mode = c("any", "all")) {
  df <- bt_as_data_frame(data)
  mode <- match.arg(mode)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  keep <- if (mode == "any") apply(df, 1L, function(x) !any(bt_is_blank(x))) else apply(df, 1L, function(x) !all(bt_is_blank(x)))
  bt_as_tibble(bt_as_data_frame(data)[keep, , drop = FALSE])
}

keepmissing <- function(data, cols, mode = c("any", "all")) {
  df <- bt_as_data_frame(data)
  mode <- match.arg(mode)
  cols <- bt_resolve_cols(df, cols)
  keep <- if (mode == "any") apply(df[, cols, drop = FALSE], 1L, function(x) any(bt_is_blank(x))) else apply(df[, cols, drop = FALSE], 1L, function(x) all(bt_is_blank(x)))
  bt_as_tibble(df[keep, , drop = FALSE])
}

filldown <- function(data, cols, by = NULL) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  if (is.null(by)) {
    for (nm in cols) {
      x <- df[[nm]]
      last <- NA
      for (i in seq_along(x)) if (!bt_is_blank(x[[i]])) last <- x[[i]] else x[[i]] <- last
      df[[nm]] <- x
    }
    return(bt_as_tibble(df))
  }
  pieces <- bt_split_by(df, by = by, keepby = TRUE)
  bt_as_tibble(data.table::rbindlist(lapply(pieces, function(piece) filldown(piece, cols)), fill = TRUE))
}

fillup <- function(data, cols, by = NULL) {
  reverse(filldown(reverse(data), cols = cols, by = by))
}

fillboth <- function(data, cols, by = NULL) {
  fillup(filldown(data, cols = cols, by = by), cols = cols, by = by)
}

missingindicator <- function(data, cols = NULL) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  out <- lapply(df, function(x) as.integer(bt_is_blank(x)))
  bt_as_tibble(as.data.frame(out, stringsAsFactors = FALSE))
}

trim <- function(x) trimws(x)
squish <- function(x) gsub("\\s+", " ", trimws(x))
lower <- function(x) tolower(x)
upper <- function(x) toupper(x)
titlecase <- function(x) tools::toTitleCase(tolower(x))
sentencecase <- function(x) paste0(toupper(substr(tolower(x), 1, 1)), substring(tolower(x), 2))
textlen <- function(x) nchar(x, type = "chars", allowNA = TRUE)
left <- function(x, n) substr(x, 1L, n)
right <- function(x, n) substr(x, pmax(1L, nchar(x) - n + 1L), nchar(x))
middle <- function(x, start, end) substr(x, start, end)
truncate <- function(x, n, ellipsis = "...") ifelse(nchar(x) > n, paste0(substr(x, 1L, n - nchar(ellipsis)), ellipsis), x)
padleft <- function(x, width, pad = " ") sprintf(paste0("%", width, "s"), x)
padright <- function(x, width, pad = " ") sprintf(paste0("%-", width, "s"), x)
padcenter <- function(x, width, pad = " ") {
  x <- as.character(x)
  vapply(x, function(s) {
    n <- max(width - nchar(s), 0L)
    left_pad <- floor(n / 2)
    right_pad <- n - left_pad
    paste0(strrep(pad, left_pad), s, strrep(pad, right_pad))
  }, character(1))
}
contains <- function(x, pattern, fixed = FALSE) grepl(pattern, x, fixed = fixed)
matches <- function(x, pattern, fixed = FALSE) grepl(paste0("^", pattern, "$"), x, fixed = fixed)
startswith <- function(x, pattern, fixed = FALSE) startsWith(x, pattern)
endswith <- function(x, pattern, fixed = FALSE) vapply(pattern, function(p) grepl(paste0(p, "$"), x, fixed = fixed), logical(length(x)))
countmatch <- function(x, pattern, fixed = FALSE) lengths(regmatches(x, gregexpr(pattern, x, fixed = fixed)))
locate <- function(x, pattern, fixed = FALSE) regexpr(pattern, x, fixed = fixed)
locateall <- function(x, pattern, fixed = FALSE) gregexpr(pattern, x, fixed = fixed)
extract <- function(x, pattern, ...) regmatches(x, regexpr(pattern, x, ...))
extractall <- function(x, pattern, ...) regmatches(x, gregexpr(pattern, x, ...))
extractnum <- function(x) as.numeric(gsub("[^0-9.-]+", "", x))
extractint <- function(x) as.integer(gsub("[^0-9-]+", "", x))
extractbetween <- function(x, left, right) sub(paste0(".*", left, "(.*)", right, ".*"), "\\1", x)
replacetext <- function(x, pattern, replacement, fixed = FALSE) gsub(pattern, replacement, x, fixed = fixed)
replaceall <- function(x, old, new) replacevalues(x, old, new)
removetext <- function(x, pattern, fixed = FALSE) gsub(pattern, "", x, fixed = fixed)
removeall <- function(x, pattern, fixed = FALSE) gsub(pattern, "", x, fixed = fixed)
splittext <- function(x, sep, fixed = FALSE) strsplit(x, sep, fixed = fixed)
splitfirst <- function(x, sep, fixed = FALSE) vapply(strsplit(x, sep, fixed = fixed), function(z) z[[1L]], character(1))
splitlast <- function(x, sep, fixed = FALSE) vapply(strsplit(x, sep, fixed = fixed), function(z) tail(z, 1L), character(1))
jointext <- function(...) paste(..., sep = "")
collapsetext <- function(x, sep = "") paste(x, collapse = sep)
isblank <- function(x) bt_is_blank(x)
removeaccents <- function(x) iconv(x, from = "", to = "ASCII//TRANSLIT")
normalizeunicode <- function(x) x
normalizeencoding <- function(x) enc2utf8(x)
transliterate <- function(x) iconv(x, from = "", to = "ASCII//TRANSLIT")
textdist <- function(x, y) utils::adist(x, y)
nearesttext <- function(x, choices) choices[apply(textdist(x, choices), 1L, which.min)]
similartext <- function(x, choices) nearesttext(x, choices)
isalpha <- function(x) grepl("^[[:alpha:]]+$", x)
isalphanumeric <- function(x) grepl("^[[:alnum:]]+$", x)
isnumerictext <- function(x) grepl("^[+-]?([0-9]*[.])?[0-9]+$", x)
isintegertext <- function(x) grepl("^[+-]?[0-9]+$", x)
isemail <- function(x) grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", x)
isurl <- function(x) grepl("^(https?|ftp)://", x)
recode <- function(x, old, new) replacevalues(x, old, new)
collapsevalues <- function(x, groups) {
  out <- x
  for (nm in names(groups)) out[x %in% groups[[nm]]] <- nm
  out
}
parsefailures <- function(x, fun, ...) {
  parsed <- tryCatch(fun(x, ...), error = function(e) rep(NA, length(x)))
  failed <- is.na(parsed) & !is.na(x)
  bt_as_tibble(data.frame(index = which(failed), value = x[failed], stringsAsFactors = FALSE))
}
parseint <- function(x, na = character(), strict = FALSE) {
  x[x %in% na] <- NA
  out <- suppressWarnings(as.integer(x))
  if (strict && any(!is.na(x) & is.na(out))) stop("Parse failure.", call. = FALSE)
  out
}
parsenum <- function(x, decimal = ".", grouping = ",", na = character(), strict = FALSE) {
  x[x %in% na] <- NA
  x <- gsub(grouping, "", x, fixed = TRUE)
  if (decimal != ".") x <- sub(decimal, ".", x, fixed = TRUE)
  out <- suppressWarnings(as.numeric(x))
  if (strict && any(!is.na(x) & is.na(out))) stop("Parse failure.", call. = FALSE)
  out
}
parselogical <- function(x, na = character(), strict = FALSE) {
  x[x %in% na] <- NA
  map <- c("true" = TRUE, "t" = TRUE, "1" = TRUE, "false" = FALSE, "f" = FALSE, "0" = FALSE)
  out <- unname(map[tolower(x)])
  if (strict && any(!is.na(x) & is.na(out))) stop("Parse failure.", call. = FALSE)
  out
}
parsedate <- function(x, formats, tz = "UTC", strict = FALSE) {
  out <- rep(as.Date(NA), length(x))
  for (fmt in formats) {
    idx <- is.na(out) & !is.na(x)
    if (!any(idx)) break
    parsed <- as.Date(x[idx], format = fmt)
    out[idx] <- ifelse(is.na(parsed), out[idx], parsed)
  }
  if (strict && any(!is.na(x) & is.na(out))) stop("Parse failure.", call. = FALSE)
  out
}
parsedatetime <- function(x, formats, tz = "UTC", strict = FALSE) {
  out <- rep(as.POSIXct(NA), length(x))
  for (fmt in formats) {
    idx <- is.na(out) & !is.na(x)
    if (!any(idx)) break
    parsed <- as.POSIXct(x[idx], format = fmt, tz = tz)
    out[idx] <- ifelse(is.na(parsed), out[idx], parsed)
  }
  if (strict && any(!is.na(x) & is.na(out))) stop("Parse failure.", call. = FALSE)
  out
}
parsepercent <- function(x, strict = FALSE) parsenum(sub("%$", "", x), strict = strict) / 100
parsecurrency <- function(x, strict = FALSE) parsenum(gsub("[$,]", "", x), strict = strict)
convertcols <- function(data, cols, fun, ...) applycols(data, cols = cols, fun = function(x, ...) fun(x, ...), ...)
collapselevels <- function(x, groups) collapsevalues(as.character(x), groups)
lump <- function(x, n = 5, other = "Other") {
  tab <- sort(table(x), decreasing = TRUE)
  keep <- names(tab)[seq_len(min(n, length(tab)))]
  out <- as.character(x)
  out[!out %in% keep] <- other
  out
}
reorderlevels <- function(x, by) factor(x, levels = by)
expandlevels <- function(x, levels) factor(x, levels = unique(c(levels, base::levels(x))))
nalevel <- function(x, value = "Missing") ifelse(is.na(x), value, as.character(x))
year <- function(x) as.integer(format(as.Date(x), "%Y"))
month <- function(x) as.integer(format(as.Date(x), "%m"))
day <- function(x) as.integer(format(as.Date(x), "%d"))
weekday <- function(x) weekdays(as.Date(x))
yearday <- function(x) as.integer(format(as.Date(x), "%j"))
week <- function(x) as.integer(format(as.Date(x), "%U"))
quarter <- function(x) (month(x) - 1L) %/% 3L + 1L
hour <- function(x) as.integer(format(as.POSIXct(x), "%H"))
minute <- function(x) as.integer(format(as.POSIXct(x), "%M"))
second <- function(x) as.integer(format(as.POSIXct(x), "%S"))
floordate <- function(x, unit = c("day", "week", "month", "year")) as.Date(x)
ceilingdate <- function(x, unit = c("day", "week", "month", "year")) as.Date(x)
rounddate <- function(x, unit = c("day", "week", "month", "year")) as.Date(x)
adddays <- function(x, n) as.Date(x) + n
addweeks <- function(x, n) as.Date(x) + 7 * n
addmonths <- function(x, n, invalid = c("previous", "next", "missing", "error")) seq.Date(as.Date(x), by = paste(n, "month"), length.out = 1L)[1L]
addyears <- function(x, n, invalid = c("previous", "next", "missing", "error")) seq.Date(as.Date(x), by = paste(n, "year"), length.out = 1L)[1L]
datediff <- function(x, y, units = "days") as.numeric(as.Date(x) - as.Date(y))
dateseq <- function(from, to, by = "day", length.out = NULL) seq.Date(as.Date(from), as.Date(to), by = by, length.out = length.out)
betweendates <- function(x, start, end) as.Date(x) >= as.Date(start) & as.Date(x) <= as.Date(end)
rescale <- function(x, to = c(0, 1)) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1L]) / diff(rng) * diff(to) + to[1L]
}
standardize <- function(x) (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
center <- function(x) x - mean(x, na.rm = TRUE)
clamp <- function(x, lower = -Inf, upper = Inf) pmin(pmax(x, lower), upper)
winsorize <- function(x, probs = c(0.01, 0.99)) {
  qs <- stats::quantile(x, probs = probs, na.rm = TRUE)
  clamp(x, qs[[1L]], qs[[2L]])
}
quantilegroup <- function(x, n = 5) cut(x, breaks = stats::quantile(x, probs = seq(0, 1, length.out = n + 1L), na.rm = TRUE), include.lowest = TRUE)
percentchange <- function(x) c(NA, diff(x) / head(x, -1L) * 100)
assertnames <- function(data, names) {
  if (!all(names %in% base::names(bt_as_data_frame(data)))) stop("Missing required names.", call. = FALSE)
  invisible(data)
}
assertcols <- function(data, cols) assert_cols(data, cols)
assertkey <- function(data, by) assert_key(data, by)
assertrows <- function(data, condition) {
  df <- bt_as_data_frame(data)
  cond <- bt_eval_logical(substitute(condition), df, nrow(df))
  if (!all(cond)) stop("Some rows failed the assertion.", call. = FALSE)
  invisible(data)
}
assertvalues <- function(data, column, allowed) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  if (length(column) != 1L) stop("`column` must be one column.", call. = FALSE)
  bad <- !df[[column]] %in% allowed & !is.na(df[[column]])
  if (any(bad)) stop("Some values are not allowed.", call. = FALSE)
  invisible(data)
}
assertrange <- function(data, column, lower, upper) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  x <- df[[column]]
  if (any(!is.na(x) & (x < lower | x > upper))) stop("Values out of range.", call. = FALSE)
  invisible(data)
}
asserttype <- function(data, column, class) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  if (!inherits(df[[column]], class)) stop("Unexpected type.", call. = FALSE)
  invisible(data)
}
assertunique <- function(data, cols) {
  if (anyDuplicated(bt_as_data_frame(data)[, bt_resolve_cols(bt_as_data_frame(data), cols), drop = FALSE])) stop("Values are not unique.", call. = FALSE)
  invisible(data)
}
assertcomplete <- function(data, cols = NULL) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  if (any(!stats::complete.cases(df))) stop("Missing values found.", call. = FALSE)
  invisible(data)
}
invalidrows <- function(data, condition) {
  df <- bt_as_data_frame(data)
  cond <- bt_eval_logical(substitute(condition), df, nrow(df))
  bt_as_tibble(df[!cond, , drop = FALSE])
}
invalidvalues <- function(data, column, allowed) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  bt_as_tibble(df[!df[[column]] %in% allowed & !is.na(df[[column]]), , drop = FALSE])
}
outofrange <- function(data, column, lower, upper) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  bt_as_tibble(df[!is.na(df[[column]]) & (df[[column]] < lower | df[[column]] > upper), , drop = FALSE])
}
equaldata <- function(x, y, ignoreorder = FALSE, ignorerownames = TRUE, tolerance = sqrt(.Machine$double.eps)) {
  x_df <- bt_as_data_frame(x); y_df <- bt_as_data_frame(y)
  if (ignoreorder) {
    x_df <- x_df[do.call(order, x_df), , drop = FALSE]
    y_df <- y_df[do.call(order, y_df), , drop = FALSE]
  }
  isTRUE(all.equal(x_df, y_df, tolerance = tolerance, check.attributes = !ignorerownames))
}
sameschema <- function(x, y) identical(names(bt_as_data_frame(x)), names(bt_as_data_frame(y)))
compareschema <- function(x, y) {
  x_df <- bt_as_data_frame(x); y_df <- bt_as_data_frame(y)
  cols <- union(names(x_df), names(y_df))
  bt_as_tibble(data.frame(
    column = cols,
    in_x = cols %in% names(x_df),
    in_y = cols %in% names(y_df),
    type_x = vapply(cols, function(nm) if (nm %in% names(x_df)) typeof(x_df[[nm]]) else NA_character_, character(1)),
    type_y = vapply(cols, function(nm) if (nm %in% names(y_df)) typeof(y_df[[nm]]) else NA_character_, character(1)),
    stringsAsFactors = FALSE
  ))
}
addedrows <- function(old, new, by = NULL) {
  old_df <- bt_as_data_frame(old); new_df <- bt_as_data_frame(new)
  if (is.null(by)) return(bt_as_tibble(setdiff(new_df, old_df)))
  by <- bt_resolve_cols(old_df, by); bt_resolve_cols(new_df, by)
  xk <- interaction(old_df[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  yk <- interaction(new_df[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  bt_as_tibble(new_df[!(yk %in% xk), , drop = FALSE])
}
removedrows <- function(old, new, by = NULL) addedrows(new, old, by = by)
changedrows <- function(old, new, by = NULL) {
  old_df <- bt_as_data_frame(old); new_df <- bt_as_data_frame(new)
  if (is.null(by)) return(bt_as_tibble(setdiff(rbind(old_df, new_df), intersect(old_df, new_df))))
  by <- bt_resolve_cols(old_df, by); bt_resolve_cols(new_df, by)
  bt_as_tibble(merge(old_df, new_df, by = by, suffixes = c(".old", ".new"), all = FALSE))
}
changedcols <- function(old, new, by = NULL) compareschema(old, new)
compare <- function(x, y, by = NULL) {
  x_df <- bt_as_data_frame(x)
  y_df <- bt_as_data_frame(y)
  out <- list(
    dims = data.frame(object = c("x", "y"), rows = c(nrow(x_df), nrow(y_df)), cols = c(ncol(x_df), ncol(y_df)), stringsAsFactors = FALSE),
    names = data.frame(column = union(names(x_df), names(y_df)), in_x = union(names(x_df), names(y_df)) %in% names(x_df), in_y = union(names(x_df), names(y_df)) %in% names(y_df), stringsAsFactors = FALSE),
    types = merge(types(x_df), types(y_df), by = "column", all = TRUE, suffixes = c(".x", ".y")),
    missing = merge(missingness(x_df, margin = "column"), missingness(y_df, margin = "column"), by = "column", all = TRUE, suffixes = c(".x", ".y")),
    schema = compareschema(x, y),
    equal = equaldata(x, y),
    relationship = if (is.null(by)) NA_character_ else joinrelationship(x, y, by)
  )
  if (!is.null(by)) {
    by <- bt_resolve_cols(x_df, by)
    bt_resolve_cols(y_df, by)
    x_keys <- unique(x_df[, by, drop = FALSE])
    y_keys <- unique(y_df[, by, drop = FALSE])
    out$key_overlap <- data.frame(x_unique = nrow(x_keys), y_unique = nrow(y_keys), common = nrow(merge(x_keys, y_keys, by = by)), stringsAsFactors = FALSE)
  }
  lapply(out, function(x) if (inherits(x, "data.frame")) bt_as_tibble(x) else x)
}
