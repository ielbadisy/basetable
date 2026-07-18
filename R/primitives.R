`%||%` <- function(x, y) if (is.null(x)) y else x

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
  do.call(pmin, c(as.list(df), na.rm = na.rm))
}

rowmax <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  do.call(pmax, c(as.list(df), na.rm = na.rm))
}

rowany <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  mat <- as.matrix(df)
  if (na.rm) {
    mat[is.na(mat)] <- FALSE
    return(rowSums(mat) > 0)
  }
  has_na <- rowSums(is.na(mat)) > 0
  mat[is.na(mat)] <- FALSE
  out <- rowSums(mat) > 0
  out[has_na & !out] <- NA
  out
}

rowall <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  mat <- as.matrix(df)
  if (na.rm) {
    mat[is.na(mat)] <- TRUE
    return(rowSums(!mat) == 0)
  }
  has_na <- rowSums(is.na(mat)) > 0
  mat[is.na(mat)] <- TRUE
  out <- rowSums(!mat) == 0
  out[has_na & out] <- NA
  out
}

rowcount <- function(data, cols = NULL, value = TRUE, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  eqmat <- sapply(df, function(x) x == value)
  if (is.null(dim(eqmat))) eqmat <- matrix(eqmat, nrow = nrow(df))
  as.integer(rowSums(eqmat, na.rm = na.rm))
}

rowfirst <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  mat <- as.matrix(df)
  if (!na.rm) return(mat[, 1L])
  nonna <- !is.na(mat)
  idx <- max.col(nonna, ties.method = "first")
  out <- mat[cbind(seq_len(nrow(mat)), idx)]
  out[rowSums(nonna) == 0] <- NA
  out
}

rowlast <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  mat <- as.matrix(df)
  if (!na.rm) return(mat[, ncol(mat)])
  nonna <- !is.na(mat)
  idx_rev <- max.col(nonna[, rev(seq_len(ncol(nonna))), drop = FALSE], ties.method = "first")
  idx <- ncol(mat) - idx_rev + 1L
  out <- mat[cbind(seq_len(nrow(mat)), idx)]
  out[rowSums(nonna) == 0] <- NA
  out
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

propcount <- function(data, by, margin = NULL) {
  out <- count(data, by = by, sort = FALSE, name = "n")
  if (is.null(margin)) {
    out$prop <- out$n / sum(out$n)
    return(out)
  }
  margin <- bt_resolve_cols(out, margin)
  out_dt <- data.table::as.data.table(out)
  out_dt[, prop := n / sum(n), by = margin]
  bt_as_tibble(out_dt)
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

unmatchedkeys <- function(x, y, by) {
  x_dt <- bt_as_data_table_ro(x)
  y_dt <- bt_as_data_table_ro(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  y_keys <- unique(y_dt[, by, with = FALSE])
  unique(x_dt, by = by)[!y_keys, on = by]
}

matchedkeys <- function(x, y, by) {
  x_dt <- bt_as_data_table_ro(x)
  y_dt <- bt_as_data_table_ro(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  y_keys <- unique(y_dt[, by, with = FALSE])
  x_dt[y_keys, on = by, nomatch = NULL]
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

nearestmerge <- function(x, y, by, tolerance = Inf) {
  rollingmerge(x, y, by = by, direction = "nearest", tolerance = tolerance)
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

fillboth <- function(data, cols, by = NULL) {
  fillup(filldown(data, cols = cols, by = by), cols = cols, by = by)
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
  old_dt <- bt_as_data_table(old_df); new_dt <- bt_as_data_table(new_df)
  old_keys <- unique(old_dt[, by, with = FALSE])
  bt_as_tibble(new_dt[!old_keys, on = by])
}
removedrows <- function(old, new, by = NULL) addedrows(new, old, by = by)
changedrows <- function(old, new, by = NULL) {
  old_df <- bt_as_data_frame(old); new_df <- bt_as_data_frame(new)
  if (is.null(by)) return(bt_as_tibble(setdiff(rbind(old_df, new_df), intersect(old_df, new_df))))
  by <- bt_resolve_cols(old_df, by); bt_resolve_cols(new_df, by)
  bt_as_tibble(merge(old_df, new_df, by = by, suffixes = c(".old", ".new"), all = FALSE))
}
changedcols <- function(old, new, by = NULL) compareschema(old, new)
