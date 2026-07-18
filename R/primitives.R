`%||%` <- function(x, y) if (is.null(x)) y else x

#' Column names
#'
#' @param data A data.frame or data.table.
#'
#' @return A character vector of column names.
#' @export
colnames <- function(data) names(bt_as_data_frame(data))

#' Row names
#'
#' @param data A data.frame or data.table.
#'
#' @return A character vector of row names.
#' @export
rownames <- function(data) base::rownames(bt_as_data_frame(data))

#' Column classes
#'
#' @param data A data.frame or data.table.
#'
#' @return A tibble with one row per column giving its class.
#' @export
classes <- function(data) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(data.frame(column = names(df), class = vapply(df, bt_mode, character(1)), stringsAsFactors = FALSE))
}

#' Count of distinct values per column
#'
#' @param data A data.frame or data.table.
#'
#' @return A named integer vector of distinct-value counts.
#' @export
uniques <- function(data) {
  df <- bt_as_data_frame(data)
  stats::setNames(vapply(df, bt_distinct_n, integer(1)), names(df))
}

#' Distinct-value count (or proportion) per column
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param prop Return the proportion of distinct values instead of the count.
#'
#' @return A named numeric vector.
#' @export
cardinality <- function(data, cols, prop = FALSE) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  out <- vapply(cols, function(nm) {
    n <- bt_distinct_n(df[[nm]])
    if (prop) n / nrow(df) else n
  }, numeric(1))
  stats::setNames(out, cols)
}

#' Columns with a single distinct non-missing value
#'
#' @param data A data.frame or data.table.
#'
#' @return A character vector of constant column names.
#' @export
constants <- function(data) {
  df <- bt_as_data_frame(data)
  keep <- vapply(df, function(x) length(unique(stats::na.omit(x))) == 1L, logical(1))
  names(df)[keep]
}

#' Columns that are entirely blank or missing
#'
#' @param data A data.frame or data.table.
#'
#' @return A character vector of column names.
#' @export
emptycols <- function(data) {
  df <- bt_as_data_frame(data)
  names(df)[vapply(df, function(x) all(bt_is_blank(x)), logical(1)) | vapply(df, function(x) all(is.na(x)), logical(1))]
}

#' Rows that are entirely blank or missing
#'
#' @param data A data.frame or data.table.
#'
#' @return A data.frame of the fully blank rows.
#' @export
emptyrows <- function(data) {
  df <- bt_as_data_frame(data)
  mask <- do.call(cbind, lapply(df, bt_is_blank))
  keep <- rowSums(mask) == ncol(df)
  df[keep, , drop = FALSE]
}

#' Rows involved in a duplicate
#'
#' @param data A data.frame or data.table.
#'
#' @return A data.frame of the duplicated rows.
#' @export
duplicaterows <- function(data) {
  df <- bt_as_data_frame(data)
  df[duplicated(df) | duplicated(df, fromLast = TRUE), , drop = FALSE]
}

#' Duplicated key combinations
#'
#' @param data A data.frame or data.table.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return See [duplicated_keys()].
#' @export
duplicatekeys <- function(data, by) {
  duplicated_keys(data, by)
}

#' Duplicated column names
#'
#' @param data A data.frame or data.table.
#'
#' @return A character vector of the duplicated names.
#' @export
duplicatenames <- function(data) {
  df <- bt_as_data_frame(data)
  names(df)[duplicated(names(df))]
}

#' Column names shared by two tables
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#'
#' @return See [common_names()].
#' @export
commonnames <- function(x, y) {
  common_names(x, y)
}

#' Clean column names to a unique, syntactic form
#'
#' @param data A data.frame or data.table.
#'
#' @return `data` with cleaned column names.
#' @export
cleannames <- function(data) {
  df <- bt_as_data_frame(data)
  names(df) <- bt_clean_names(names(df), method = "unique")
  bt_as_tibble(df)
}

#' Repair column names
#'
#' @param data A data.frame or data.table.
#' @param method Cleaning strategy to apply to names.
#'
#' @return `data` with repaired column names.
#' @export
repairnames <- function(data, method = c("unique", "universal", "minimal")) {
  df <- bt_as_data_frame(data)
  names(df) <- bt_clean_names(names(df), method = match.arg(method))
  bt_as_tibble(df)
}

#' Rename columns with a function
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param fun Function applied to each element, column, or group.
#'
#' @return `data` with the selected columns renamed.
#' @export
renamewith <- function(data, cols, fun) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  names(df)[match(cols, names(df))] <- vapply(cols, function(x) fun(x), character(1))
  bt_as_tibble(df)
}

#' Move columns before or after another column
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param before Column identifying where to insert `cols` before.
#' @param after Column identifying where to insert `cols` after.
#'
#' @return `data` with columns reordered.
#' @export
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

#' Move columns to the front
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#'
#' @return `data` with `cols` moved first.
#' @export
firstcols <- function(data, cols) move(data, cols, before = 1L)

#' Move columns to the back
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#'
#' @return `data` with `cols` moved last.
#' @export
lastcols <- function(data, cols) move(data, cols, after = ncol(bt_as_data_frame(data)) - length(bt_resolve_cols(bt_as_data_frame(data), cols)))

#' First `n` rows
#'
#' @param data A data.frame or data.table.
#' @param n Integer count.
#'
#' @return The first `n` rows of `data`.
#' @export
firstrows <- function(data, n = 1L) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(utils::head(df, n))
}

#' Last `n` rows
#'
#' @param data A data.frame or data.table.
#' @param n Integer count.
#'
#' @return The last `n` rows of `data`.
#' @export
lastrows <- function(data, n = 1L) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(utils::tail(df, n))
}

#' Sample `n` rows without replacement
#'
#' @param data A data.frame or data.table.
#' @param n Integer count.
#'
#' @return A random sample of `n` rows.
#' @export
samplerows <- function(data, n) {
  df <- bt_as_data_frame(data)
  idx <- sample.int(nrow(df), n)
  bt_as_tibble(df[idx, , drop = FALSE])
}

#' Sample a fraction of rows without replacement
#'
#' @param data A data.frame or data.table.
#' @param frac Fraction of rows to sample.
#'
#' @return A random sample of rows.
#' @export
samplefrac <- function(data, frac) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(df[sample.int(nrow(df), ceiling(nrow(df) * frac)), , drop = FALSE])
}

#' Order rows by one or more columns
#'
#' @param data A data.frame or data.table.
#' @param by Character vector of column names identifying groups or join keys.
#' @param decreasing Sort in decreasing order.
#' @param na.last Placement of missing values when sorting.
#'
#' @return `data` sorted by `by`.
#' @export
orderrows <- function(data, by, decreasing = FALSE, na.last = TRUE) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(bt_order_data(df, by, decreasing = decreasing, na.last = na.last))
}

#' Reverse row order
#'
#' @param data A data.frame or data.table.
#'
#' @return `data` with rows in reverse order.
#' @export
reverse <- function(data) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(df[rev(seq_len(nrow(df))), , drop = FALSE])
}

#' First row within each group
#'
#' @param data A data.frame or data.table.
#' @param by Character vector of column names identifying groups or join keys.
#' @param order Optional column(s) used to break ties before picking first/last rows.
#'
#' @return One row per group.
#' @export
firstby <- function(data, by, order = NULL) {
  df <- bt_as_data_frame(data)
  if (!is.null(order)) df <- bt_order_data(df, order)
  df <- df[!duplicated(df[, bt_resolve_cols(df, by), drop = FALSE]), , drop = FALSE]
  bt_as_tibble(df)
}

#' Last row within each group
#'
#' @param data A data.frame or data.table.
#' @param by Character vector of column names identifying groups or join keys.
#' @param order Optional column(s) used to break ties before picking first/last rows.
#'
#' @return One row per group.
#' @export
lastby <- function(data, by, order = NULL) {
  df <- bt_as_data_frame(data)
  if (!is.null(order)) df <- bt_order_data(df, order)
  key <- bt_resolve_cols(df, by)
  keep <- !duplicated(df[, key, drop = FALSE], fromLast = TRUE)
  bt_as_tibble(df[keep, , drop = FALSE])
}

#' Remove duplicate rows, optionally by key
#'
#' @param data A data.frame or data.table.
#' @param by Character vector of column names identifying groups or join keys.
#' @param keep Which duplicate to keep.
#'
#' @return `data` with duplicates removed.
#' @export
removeduplicates <- function(data, by = NULL, keep = c("first", "last", "none")) {
  keep <- match.arg(keep)
  if (is.null(by)) {
    df <- bt_as_data_frame(data)
    return(bt_as_tibble(if (keep == "first") df[!duplicated(df), , drop = FALSE] else if (keep == "last") df[!duplicated(df, fromLast = TRUE), , drop = FALSE] else df[FALSE, , drop = FALSE]))
  }
  if (keep == "first") return(distinct(data, cols = by, .keep_all = TRUE))
  if (keep == "last") return(lastby(data, by = by))
  df <- bt_as_data_frame(data)
  key <- bt_resolve_cols(df, by)
  bt_as_tibble(df[!duplicated(df[, key, drop = FALSE]) & !duplicated(df[, key, drop = FALSE], fromLast = TRUE), , drop = FALSE])
}

#' Row-wise minimum across columns
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A numeric vector of row minimums.
#' @export
rowmin <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  do.call(pmin, c(as.list(df), na.rm = na.rm))
}

#' Row-wise maximum across columns
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A numeric vector of row maximums.
#' @export
rowmax <- function(data, cols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  do.call(pmax, c(as.list(df), na.rm = na.rm))
}

#' Row-wise `any()` across columns
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A logical vector.
#' @export
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

#' Row-wise `all()` across columns
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A logical vector.
#' @export
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

#' Count matches of a value across columns, per row
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param value Value to test, assign, or match against.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return An integer vector.
#' @export
rowcount <- function(data, cols = NULL, value = TRUE, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  eqmat <- sapply(df, function(x) x == value)
  if (is.null(dim(eqmat))) eqmat <- matrix(eqmat, nrow = nrow(df))
  as.integer(rowSums(eqmat, na.rm = na.rm))
}

#' First non-missing value across columns, per row
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A vector of first values.
#' @export
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

#' Last non-missing value across columns, per row
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A vector of last values.
#' @export
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

#' Apply a function row-wise
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param fun Function applied to each element, column, or group.
#' @param ... Additional arguments (unused, or passed through depending on the function).
#'
#' @return The result of `fun` applied to each row.
#' @export
rowapply <- function(data, cols = NULL, fun, ...) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  apply(df, 1L, fun, ...)
}

#' Lag a vector
#'
#' @param x An atomic vector.
#' @param n Integer count.
#' @param default Value used to fill positions with no lagged observation.
#'
#' @return `x` shifted forward by `n` positions.
#' @export
lagvalue <- function(x, n = 1L, default = NA) {
  c(rep(default, n), utils::head(x, -n))
}

#' Lead a vector
#'
#' @param x An atomic vector.
#' @param n Integer count.
#' @param default Value used to fill positions with no leading observation.
#'
#' @return `x` shifted backward by `n` positions.
#' @export
leadvalue <- function(x, n = 1L, default = NA) {
  c(utils::tail(x, -n), rep(default, n))
}

#' Lagged difference
#'
#' @param x An atomic vector.
#' @param lag Lag used for the difference.
#'
#' @return A numeric vector of differences.
#' @export
difference <- function(x, lag = 1L) {
  c(rep(NA, lag), diff(x, lag = lag))
}

#' Row position
#'
#' @param x An atomic vector.
#'
#' @return An integer sequence along `x`.
#' @export
rownumber <- function(x) seq_along(x)

#' Dense rank of a vector
#'
#' @param x An atomic vector.
#'
#' @return An integer vector of dense ranks.
#' @export
denserank <- function(x) match(x, unique(x))

#' Percentile rank of a vector
#'
#' @param x An atomic vector.
#'
#' @return A numeric vector of percentile ranks in `[0, 1]`.
#' @export
percentrank <- function(x) rank(x, ties.method = "average", na.last = "keep") / sum(!is.na(x))

#' Cumulative empirical distribution
#'
#' @param x An atomic vector.
#'
#' @return A numeric vector.
#' @export
cumedist <- function(x) cumsum(!duplicated(x)) / seq_along(x)

#' Cumulative mean
#'
#' @param x An atomic vector.
#'
#' @return A numeric vector.
#' @export
cummean <- function(x) cumsum(x) / seq_along(x)

#' Cumulative row counter
#'
#' @param x An atomic vector.
#'
#' @return An integer sequence along `x`.
#' @export
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

bt_roll_apply <- function(x, width, FUN, align = c("right", "left", "center"), fill = NA, partial = TRUE, ...) {
  align <- match.arg(align)
  idxs <- bt_roll_window(length(x), width, align)
  out <- vapply(idxs, function(idx) {
    if (length(idx) == 0L || (!partial && length(idx) < width)) return(fill)
    FUN(x[idx], ...)
  }, FUN.VALUE = FUN(x[seq_len(min(width, length(x)))], ...))
  out
}

#' Rolling mean
#'
#' @param x An atomic vector.
#' @param width Target width, in characters or rolling-window size depending on the function.
#' @param align Alignment of the rolling window relative to each position.
#' @param fill Value used for positions where no window/result is available.
#' @param partial Allow partial windows at the boundaries.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A numeric vector of rolling means.
#' @export
rollmean <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollmean", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollmean(x, n = width, align = match.arg(align), fill = fill, na.rm = na.rm, partial = partial))
  bt_roll_apply(x, width, mean, align = align, fill = fill, partial = partial, na.rm = na.rm)
}

#' Rolling sum
#'
#' @param x An atomic vector.
#' @param width Target width, in characters or rolling-window size depending on the function.
#' @param align Alignment of the rolling window relative to each position.
#' @param fill Value used for positions where no window/result is available.
#' @param partial Allow partial windows at the boundaries.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A numeric vector of rolling sums.
#' @export
rollsum <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollsum", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollsum(x, n = width, align = match.arg(align), fill = fill, na.rm = na.rm, partial = partial))
  bt_roll_apply(x, width, sum, align = align, fill = fill, partial = partial, na.rm = na.rm)
}

#' Rolling minimum
#'
#' @param x An atomic vector.
#' @param width Target width, in characters or rolling-window size depending on the function.
#' @param align Alignment of the rolling window relative to each position.
#' @param fill Value used for positions where no window/result is available.
#' @param partial Allow partial windows at the boundaries.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A numeric vector of rolling minimums.
#' @export
rollmin <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollmin", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollmin(x, n = width, align = match.arg(align), fill = fill, na.rm = na.rm, partial = partial))
  bt_roll_apply(x, width, min, align = align, fill = fill, partial = partial, na.rm = na.rm)
}

#' Rolling maximum
#'
#' @param x An atomic vector.
#' @param width Target width, in characters or rolling-window size depending on the function.
#' @param align Alignment of the rolling window relative to each position.
#' @param fill Value used for positions where no window/result is available.
#' @param partial Allow partial windows at the boundaries.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A numeric vector of rolling maximums.
#' @export
rollmax <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollmax", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollmax(x, n = width, align = match.arg(align), fill = fill, na.rm = na.rm, partial = partial))
  bt_roll_apply(x, width, max, align = align, fill = fill, partial = partial, na.rm = na.rm)
}

#' Rolling median
#'
#' @param x An atomic vector.
#' @param width Target width, in characters or rolling-window size depending on the function.
#' @param align Alignment of the rolling window relative to each position.
#' @param fill Value used for positions where no window/result is available.
#' @param partial Allow partial windows at the boundaries.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A numeric vector of rolling medians.
#' @export
rollmedian <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollmedian", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollmedian(x, n = width, align = match.arg(align), fill = fill, na.rm = na.rm, partial = partial))
  bt_roll_apply(x, width, stats::median, align = align, fill = fill, partial = partial, na.rm = na.rm)
}

#' Rolling standard deviation
#'
#' @param x An atomic vector.
#' @param width Target width, in characters or rolling-window size depending on the function.
#' @param align Alignment of the rolling window relative to each position.
#' @param fill Value used for positions where no window/result is available.
#' @param partial Allow partial windows at the boundaries.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A numeric vector of rolling standard deviations.
#' @export
rollsd <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollsd", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollsd(x, n = width, align = match.arg(align), fill = fill, na.rm = na.rm, partial = partial))
  bt_roll_apply(x, width, stats::sd, align = align, fill = fill, partial = partial, na.rm = na.rm)
}

#' Rolling variance
#'
#' @param x An atomic vector.
#' @param width Target width, in characters or rolling-window size depending on the function.
#' @param align Alignment of the rolling window relative to each position.
#' @param fill Value used for positions where no window/result is available.
#' @param partial Allow partial windows at the boundaries.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A numeric vector of rolling variances.
#' @export
rollvar <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollvar", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollvar(x, n = width, align = match.arg(align), fill = fill, na.rm = na.rm, partial = partial))
  bt_roll_apply(x, width, stats::var, align = align, fill = fill, partial = partial, na.rm = na.rm)
}

#' Rolling product
#'
#' @param x An atomic vector.
#' @param width Target width, in characters or rolling-window size depending on the function.
#' @param align Alignment of the rolling window relative to each position.
#' @param fill Value used for positions where no window/result is available.
#' @param partial Allow partial windows at the boundaries.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A numeric vector of rolling products.
#' @export
rollprod <- function(x, width, align = c("right", "left", "center"), fill = NA, partial = FALSE, na.rm = FALSE) {
  if (exists("frollprod", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollprod(x, n = width, align = match.arg(align), fill = fill, na.rm = na.rm, partial = partial))
  bt_roll_apply(x, width, prod, align = align, fill = fill, partial = partial, na.rm = na.rm)
}

#' Rolling window apply
#'
#' @param x An atomic vector.
#' @param width Target width, in characters or rolling-window size depending on the function.
#' @param FUN Function applied to each rolling window.
#' @param ... Additional arguments (unused, or passed through depending on the function).
#' @param align Alignment of the rolling window relative to each position.
#' @param fill Value used for positions where no window/result is available.
#' @param partial Allow partial windows at the boundaries.
#'
#' @return A vector with one result per window.
#' @export
rollapply <- function(x, width, FUN, ..., align = c("right", "left", "center"), fill = NA, partial = FALSE) {
  if (exists("frollapply", asNamespace("data.table"), inherits = FALSE)) return(data.table::frollapply(x, N = width, FUN = FUN, ..., align = match.arg(align), fill = fill, partial = partial))
  bt_roll_apply(x, width, FUN, align = align, fill = fill, partial = partial, ...)
}

#' Apply a function to selected columns
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param fun Function applied to each element, column, or group.
#' @param ... Additional arguments (unused, or passed through depending on the function).
#'
#' @return `data` with the selected columns transformed.
#' @export
applycols <- function(data, cols, fun, ...) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  for (nm in cols) df[[nm]] <- fun(df[[nm]], ...)
  bt_as_tibble(df)
}

#' Replace selected columns with new values
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param values Vector or list of replacement values.
#'
#' @return `data` with the selected columns replaced.
#' @export
replacecols <- function(data, cols, values) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  if (length(values) == 1L && is.list(values[[1L]]) && !is.data.frame(values[[1L]])) values <- values[[1L]]
  if (length(values) != length(cols)) stop("`values` must match `cols`.", call. = FALSE)
  for (i in seq_along(cols)) df[[cols[[i]]]] <- values[[i]]
  bt_as_tibble(df)
}

#' Replace values in selected columns where a condition holds
#'
#' @param data A data.frame or data.table.
#' @param condition A logical expression evaluated in the context of `data`.
#' @param cols Character vector of column names.
#' @param value Value to test, assign, or match against.
#'
#' @return `data` with matching values replaced.
#' @export
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

#' Grouped counts with proportions
#'
#' @param data A data.frame or data.table.
#' @param by Character vector of column names identifying groups or join keys.
#' @param margin Column(s) defining the totals used to compute proportions.
#'
#' @return A tibble of counts (and proportions when `margin` is supplied).
#' @export
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

#' Apply a function to each group
#'
#' @param data A data.frame or data.table.
#' @param by Character vector of column names identifying groups or join keys.
#' @param fun Function applied to each element, column, or group.
#' @param ... Additional arguments (unused, or passed through depending on the function).
#' @param bind Combine the per-group results into a single table.
#' @param id Optional name for a source identifier column.
#'
#' @return A list of per-group results, or a combined table when `bind = TRUE`.
#' @export
applyby <- function(data, by, fun, ..., bind = FALSE, id = ".group") {
  pieces <- bt_split_by(data, by = by, keepby = TRUE)
  out <- lapply(pieces, function(piece) fun(piece, ...))
  if (!bind) return(out)
  recombine(out, id = id)
}

#' Recombine a list of pieces into one table
#'
#' @param x An atomic vector.
#' @param id Optional name for a source identifier column.
#'
#' @return See [combine()].
#' @export
recombine <- function(x, id = NULL) {
  combine(x, id = id)
}

#' Distinct keys of `x` absent from `y`
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return Rows of `x` whose key is not present in `y`.
#' @export
unmatchedkeys <- function(x, y, by) {
  x_dt <- bt_as_data_table_ro(x)
  y_dt <- bt_as_data_table_ro(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  y_keys <- unique(y_dt[, by, with = FALSE])
  unique(x_dt, by = by)[!y_keys, on = by]
}

#' Rows of `x` whose key is present in `y`
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return Rows of `x` whose key is present in `y`.
#' @export
matchedkeys <- function(x, y, by) {
  x_dt <- bt_as_data_table_ro(x)
  y_dt <- bt_as_data_table_ro(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  y_keys <- unique(y_dt[, by, with = FALSE])
  x_dt[y_keys, on = by, nomatch = NULL]
}

#' Cardinality of a join key relationship
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return One of `"one-to-one"`, `"one-to-many"`, `"many-to-one"`, `"many-to-many"`.
#' @export
joinrelationship <- function(x, y, by) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)
  x_dup <- any(duplicated(x_dt[, by, drop = FALSE]))
  y_dup <- any(duplicated(y_dt[, by, drop = FALSE]))
  if (!x_dup && !y_dup) "one-to-one" else if (!x_dup && y_dup) "one-to-many" else if (x_dup && !y_dup) "many-to-one" else "many-to-many"
}

#' Nearest-key join
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#' @param by Character vector of column names identifying groups or join keys.
#' @param tolerance Numeric join/comparison tolerance.
#'
#' @return See [rollingmerge()].
#' @export
nearestmerge <- function(x, y, by, tolerance = Inf) {
  rollingmerge(x, y, by = by, direction = "nearest", tolerance = tolerance)
}

#' Row-bind tables, filling missing columns
#'
#' @param ... Additional arguments (unused, or passed through depending on the function).
#' @param id Optional name for a source identifier column.
#' @param fill Value used for positions where no window/result is available.
#' @param typeconflict How to handle conflicting column types across inputs.
#'
#' @return A combined tibble.
#' @export
rbindfill <- function(..., id = NULL, fill = TRUE, typeconflict = c("error", "coerce")) {
  typeconflict <- match.arg(typeconflict)
  dots <- list(...)
  if (length(dots) == 1L && is.list(dots[[1L]]) && !inherits(dots[[1L]], "data.frame")) dots <- dots[[1L]]
  bt_as_tibble(data.table::rbindlist(lapply(dots, bt_as_data_frame), fill = fill, idcol = id))
}

#' Set union of rows
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return The union of `x` and `y`.
#' @export
unionrows <- function(x, y, by = NULL) {
  df <- rbindfill(x, y)
  if (!is.null(by)) df <- distinct(df, cols = by, .keep_all = TRUE)
  bt_as_tibble(unique(bt_as_data_frame(df)))
}

#' Set intersection of rows
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return Rows of `x` also present in `y`.
#' @export
intersectrows <- function(x, y, by = NULL) {
  if (is.null(by)) return(bt_as_tibble(intersect(bt_as_data_frame(x), bt_as_data_frame(y))))
  bt_as_tibble(matchedkeys(x, y, by))
}

#' Set difference of rows
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return Rows of `x` absent from `y`.
#' @export
diffrows <- function(x, y, by = NULL) {
  if (is.null(by)) return(bt_as_tibble(setdiff(bt_as_data_frame(x), bt_as_data_frame(y))))
  bt_as_tibble(unmatchedkeys(x, y, by))
}

#' Compare two tables' rows for equality
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return A single logical value.
#' @export
equalrows <- function(x, y, by = NULL) {
  x_df <- bt_as_data_frame(x); y_df <- bt_as_data_frame(y)
  if (is.null(by)) return(identical(x_df, y_df))
  by <- bt_resolve_cols(x_df, by); bt_resolve_cols(y_df, by)
  x_dt <- data.table::as.data.table(x_df)[, by, with = FALSE]
  y_dt <- data.table::as.data.table(y_df)[, by, with = FALSE]
  data.table::setorderv(x_dt, by)
  data.table::setorderv(y_dt, by)
  isTRUE(all.equal(x_dt, y_dt, check.attributes = FALSE))
}

#' Reshape columns into key/value rows
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param names Name for the resulting key/value column, depending on the function.
#' @param values Vector or list of replacement values.
#' @param idcols Columns to keep as row identifiers.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return A long-format tibble.
#' @export
tolong <- function(data, cols, names = "variable", values = "value", idcols = NULL, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  idcols <- if (is.null(idcols)) setdiff(names(df), cols) else bt_resolve_cols(df, idcols)
  bt_as_tibble(data.table::melt(data.table::as.data.table(df), id.vars = idcols, measure.vars = cols, variable.name = names, value.name = values, na.rm = na.rm))
}

#' Reshape rows into columns
#'
#' @param data A data.frame or data.table.
#' @param names Name for the resulting key/value column, depending on the function.
#' @param values Vector or list of replacement values.
#' @param idcols Columns to keep as row identifiers.
#' @param fun Function applied to each element, column, or group.
#' @param fill Value used for positions where no window/result is available.
#'
#' @return A wide-format tibble.
#' @export
towide <- function(data, names, values, idcols = NULL, fun = NULL, fill = NA) {
  df <- bt_as_data_frame(data)
  idcols <- if (is.null(idcols)) setdiff(names(df), c(names, values)) else bt_resolve_cols(df, idcols)
  lhs <- if (length(idcols)) paste(idcols, collapse = " + ") else "."
  bt_as_tibble(data.table::dcast(
    data.table::as.data.table(df),
    formula = stats::reformulate(names, response = lhs),
    value.var = values,
    fun.aggregate = fun %||% length,
    fill = fill
  ))
}

#' Split one column into several
#'
#' @param data A data.frame or data.table.
#' @param column Name of a single column.
#' @param into Names of the columns produced by splitting.
#' @param sep Separator string.
#' @param remove Drop the source column(s) after the operation.
#' @param extra How to handle extra pieces beyond `into`.
#' @param fill Value used for positions where no window/result is available.
#'
#' @return `data` with `column` split into `into`.
#' @export
separate <- function(data, column, into, sep, remove = TRUE, extra = c("warn", "drop", "merge"), fill = c("warn", "left", "right")) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  extra <- match.arg(extra); fill <- match.arg(fill)
  parts <- strsplit(as.character(df[[column]]), sep, fixed = FALSE)
  max_len <- length(into)
  padded <- lapply(parts, function(x) {
    length(x) <- max_len
    x
  })
  out <- as.data.frame(do.call(rbind, padded), stringsAsFactors = FALSE)
  names(out) <- into
  if (remove) df[[column]] <- NULL
  bt_as_tibble(cbind(df, out, stringsAsFactors = FALSE))
}

#' Combine several columns into one
#'
#' @param data A data.frame or data.table.
#' @param column Name of a single column.
#' @param cols Character vector of column names.
#' @param sep Separator string.
#' @param remove Drop the source column(s) after the operation.
#' @param na.rm Drop missing values before computing the result.
#'
#' @return `data` with `cols` combined into `column`.
#' @export
unite <- function(data, column, cols, sep = "_", remove = TRUE, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  new <- if (na.rm) {
    apply(df[, cols, drop = FALSE], 1L, function(x) paste(x[!is.na(x)], collapse = sep))
  } else {
    do.call(paste, c(df[cols], list(sep = sep)))
  }
  keep <- if (remove) setdiff(names(df), cols) else names(df)
  df <- df[, keep, drop = FALSE]
  df[[column]] <- new
  bt_as_tibble(df)
}

#' Transpose a table
#'
#' @param data A data.frame or data.table.
#'
#' @return A transposed tibble.
#' @export
transpose <- function(data) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(as.data.frame(t(df), stringsAsFactors = FALSE))
}

#' Recode matching values to `NA`
#'
#' @param x An atomic vector.
#' @param values Vector or list of replacement values.
#'
#' @return `x` with matches replaced by `NA`.
#' @export
naif <- function(x, values) {
  x[x %in% values] <- NA
  x
}

#' Replace `NA` with a value
#'
#' @param x An atomic vector.
#' @param value Value to test, assign, or match against.
#'
#' @return `x` with missing values replaced.
#' @export
nato <- function(x, value) {
  x[is.na(x)] <- value
  x
}

#' Recode blank strings to `NA`
#'
#' @param x An atomic vector.
#'
#' @return `x` with blanks replaced by `NA`.
#' @export
blanktona <- function(x) {
  x[bt_is_blank(x)] <- NA
  x
}

#' Replace `NA` with an empty string
#'
#' @param x An atomic vector.
#'
#' @return `x` with missing values replaced by `""`.
#' @export
natoblank <- function(x) {
  x[is.na(x)] <- ""
  x
}

#' Recode values by lookup
#'
#' @param x An atomic vector.
#' @param old Baseline data.frame or data.table ("before" state).
#' @param new Updated data.frame or data.table ("after" state), or replacement values when used for value substitution.
#'
#' @return `x` with matched values replaced.
#' @export
replacevalues <- function(x, old, new) {
  map <- stats::setNames(new, old)
  out <- x
  idx <- match(as.character(x), names(map))
  keep <- !is.na(idx)
  out[keep] <- unname(map[idx[keep]])
  out
}

#' Fill missing values both forward and backward within groups
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return `data` with `cols` filled in both directions.
#' @export
fillboth <- function(data, cols, by = NULL) {
  fillup(filldown(data, cols = cols, by = by), cols = cols, by = by)
}

#' Trim leading and trailing whitespace
#'
#' @param x An atomic vector.
#'
#' @return A trimmed character vector.
#' @export
trim <- function(x) trimws(x)
#' Trim and collapse internal whitespace
#'
#' @param x An atomic vector.
#'
#' @return A cleaned character vector.
#' @export
squish <- function(x) gsub("\\s+", " ", trimws(x))
#' Convert to lower case
#'
#' @param x An atomic vector.
#'
#' @return A lower-case character vector.
#' @export
lower <- function(x) tolower(x)
#' Convert to upper case
#'
#' @param x An atomic vector.
#'
#' @return An upper-case character vector.
#' @export
upper <- function(x) toupper(x)
#' Convert to title case
#'
#' @param x An atomic vector.
#'
#' @return A title-case character vector.
#' @export
titlecase <- function(x) tools::toTitleCase(tolower(x))
#' Convert to sentence case
#'
#' @param x An atomic vector.
#'
#' @return A sentence-case character vector.
#' @export
sentencecase <- function(x) paste0(toupper(substr(tolower(x), 1, 1)), substring(tolower(x), 2))
#' Character length
#'
#' @param x An atomic vector.
#'
#' @return An integer vector of character counts.
#' @export
textlen <- function(x) nchar(x, type = "chars", allowNA = TRUE)
#' First `n` characters
#'
#' @param x An atomic vector.
#' @param n Integer count.
#'
#' @return A character vector.
#' @export
left <- function(x, n) substr(x, 1L, n)
#' Last `n` characters
#'
#' @param x An atomic vector.
#' @param n Integer count.
#'
#' @return A character vector.
#' @export
right <- function(x, n) substr(x, pmax(1L, nchar(x) - n + 1L), nchar(x))
#' Substring between two positions
#'
#' @param x An atomic vector.
#' @param start Start bound (inclusive).
#' @param end End bound (inclusive).
#'
#' @return A character vector.
#' @export
middle <- function(x, start, end) substr(x, start, end)
#' Truncate text with an ellipsis
#'
#' @param x An atomic vector.
#' @param n Integer count.
#' @param ellipsis String appended to truncated text.
#'
#' @return A character vector.
#' @export
truncate <- function(x, n, ellipsis = "...") ifelse(nchar(x) > n, paste0(substr(x, 1L, n - nchar(ellipsis)), ellipsis), x)
#' Pad text on the left
#'
#' @param x An atomic vector.
#' @param width Target width, in characters or rolling-window size depending on the function.
#' @param pad Padding character.
#'
#' @return A character vector padded to `width`.
#' @export
padleft <- function(x, width, pad = " ") {
  x <- as.character(x)
  n <- pmax(width - nchar(x), 0L)
  paste0(strrep(pad, n), x)
}
#' Pad text on the right
#'
#' @param x An atomic vector.
#' @param width Target width, in characters or rolling-window size depending on the function.
#' @param pad Padding character.
#'
#' @return A character vector padded to `width`.
#' @export
padright <- function(x, width, pad = " ") {
  x <- as.character(x)
  n <- pmax(width - nchar(x), 0L)
  paste0(x, strrep(pad, n))
}
#' Pad text on both sides
#'
#' @param x An atomic vector.
#' @param width Target width, in characters or rolling-window size depending on the function.
#' @param pad Padding character.
#'
#' @return A character vector padded to `width`.
#' @export
padcenter <- function(x, width, pad = " ") {
  x <- as.character(x)
  vapply(x, function(s) {
    n <- max(width - nchar(s), 0L)
    left_pad <- floor(n / 2)
    right_pad <- n - left_pad
    paste0(strrep(pad, left_pad), s, strrep(pad, right_pad))
  }, character(1))
}
#' Test for a pattern match anywhere in the string
#'
#' @param x An atomic vector.
#' @param pattern A regular expression (or fixed string when `fixed = TRUE`).
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return A logical vector.
#' @export
contains <- function(x, pattern, fixed = FALSE) grepl(pattern, x, fixed = fixed)
#' Test for a full-string pattern match
#'
#' @param x An atomic vector.
#' @param pattern A regular expression (or fixed string when `fixed = TRUE`).
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return A logical vector.
#' @export
matches <- function(x, pattern, fixed = FALSE) {
  if (fixed) {
    return(!is.na(x) & x == pattern)
  }
  grepl(paste0("^", pattern, "$"), x)
}
#' Test whether strings start with a pattern
#'
#' @param x An atomic vector.
#' @param pattern A regular expression (or fixed string when `fixed = TRUE`).
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return A logical vector.
#' @export
startswith <- function(x, pattern, fixed = FALSE) {
  if (fixed) {
    return(startsWith(x, pattern))
  }
  grepl(paste0("^", pattern), x)
}
#' Test whether strings end with a pattern
#'
#' @param x An atomic vector.
#' @param pattern A regular expression (or fixed string when `fixed = TRUE`).
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return A logical vector.
#' @export
endswith <- function(x, pattern, fixed = FALSE) {
  if (fixed) {
    return(endsWith(x, pattern))
  }
  grepl(paste0(pattern, "$"), x)
}
#' Count pattern matches per string
#'
#' @param x An atomic vector.
#' @param pattern A regular expression (or fixed string when `fixed = TRUE`).
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return An integer vector.
#' @export
countmatch <- function(x, pattern, fixed = FALSE) lengths(regmatches(x, gregexpr(pattern, x, fixed = fixed)))
#' Position of the first pattern match
#'
#' @param x An atomic vector.
#' @param pattern A regular expression (or fixed string when `fixed = TRUE`).
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return An integer vector of match positions (see [regexpr()]).
#' @export
locate <- function(x, pattern, fixed = FALSE) regexpr(pattern, x, fixed = fixed)
#' Positions of all pattern matches
#'
#' @param x An atomic vector.
#' @param pattern A regular expression (or fixed string when `fixed = TRUE`).
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return A list of match positions (see [gregexpr()]).
#' @export
locateall <- function(x, pattern, fixed = FALSE) gregexpr(pattern, x, fixed = fixed)
#' Extract the first pattern match
#'
#' @param x An atomic vector.
#' @param pattern A regular expression (or fixed string when `fixed = TRUE`).
#' @param ... Additional arguments (unused, or passed through depending on the function).
#'
#' @return A character vector of matches.
#' @export
extract <- function(x, pattern, ...) regmatches(x, regexpr(pattern, x, ...))
#' Extract all pattern matches
#'
#' @param x An atomic vector.
#' @param pattern A regular expression (or fixed string when `fixed = TRUE`).
#' @param ... Additional arguments (unused, or passed through depending on the function).
#'
#' @return A list of character vectors of matches.
#' @export
extractall <- function(x, pattern, ...) regmatches(x, gregexpr(pattern, x, ...))
#' Extract a numeric value from text
#'
#' @param x An atomic vector.
#'
#' @return A numeric vector.
#' @export
extractnum <- function(x) as.numeric(gsub("[^0-9.-]+", "", x))
#' Extract an integer value from text
#'
#' @param x An atomic vector.
#'
#' @return An integer vector.
#' @export
extractint <- function(x) as.integer(gsub("[^0-9-]+", "", x))
#' Extract text between two markers
#'
#' @param x An atomic vector.
#' @param left Left marker.
#' @param right Right marker.
#'
#' @return A character vector.
#' @export
extractbetween <- function(x, left, right) sub(paste0(".*", left, "(.*)", right, ".*"), "\\1", x)
#' Replace a pattern with replacement text
#'
#' @param x An atomic vector.
#' @param pattern A regular expression (or fixed string when `fixed = TRUE`).
#' @param replacement Replacement text.
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return A character vector.
#' @export
replacetext <- function(x, pattern, replacement, fixed = FALSE) gsub(pattern, replacement, x, fixed = fixed)

#' Recode values by lookup (alias)
#'
#' @param x An atomic vector.
#' @param old Existing value(s) to replace.
#' @param new Replacement value(s) matching `old` by position.
#'
#' @return See [replacevalues()].
#' @export
replaceall <- function(x, old, new) replacevalues(x, old, new)
#' Remove text matching a pattern
#'
#' @param x An atomic vector.
#' @param pattern A regular expression (or fixed string when `fixed = TRUE`).
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return A character vector.
#' @export
removetext <- function(x, pattern, fixed = FALSE) gsub(pattern, "", x, fixed = fixed)

#' Remove text matching a pattern (alias)
#'
#' @param x An atomic vector.
#' @param pattern A regular expression (or fixed string when `fixed = TRUE`).
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return A character vector.
#' @export
removeall <- function(x, pattern, fixed = FALSE) gsub(pattern, "", x, fixed = fixed)
#' Split text on a separator
#'
#' @param x An atomic vector.
#' @param sep Separator string.
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return A list of character vectors.
#' @export
splittext <- function(x, sep, fixed = FALSE) strsplit(x, sep, fixed = fixed)
#' First piece after splitting on a separator
#'
#' @param x An atomic vector.
#' @param sep Separator string.
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return A character vector.
#' @export
splitfirst <- function(x, sep, fixed = FALSE) vapply(strsplit(x, sep, fixed = fixed), function(z) z[[1L]], character(1))
#' Last piece after splitting on a separator
#'
#' @param x An atomic vector.
#' @param sep Separator string.
#' @param fixed Use fixed (non-regex) matching instead of regular expressions.
#'
#' @return A character vector.
#' @export
splitlast <- function(x, sep, fixed = FALSE) vapply(strsplit(x, sep, fixed = fixed), function(z) utils::tail(z, 1L), character(1))
#' Concatenate values element-wise
#'
#' @param ... Additional arguments (unused, or passed through depending on the function).
#'
#' @return A character vector.
#' @export
jointext <- function(...) paste(..., sep = "")
#' Collapse a vector into a single string
#'
#' @param x An atomic vector.
#' @param sep Separator string.
#'
#' @return A single character string.
#' @export
collapsetext <- function(x, sep = "") paste(x, collapse = sep)
#' Test for blank (missing or empty) values
#'
#' @param x An atomic vector.
#'
#' @return A logical vector.
#' @export
isblank <- function(x) bt_is_blank(x)
#' Strip accents from text
#'
#' @param x An atomic vector.
#'
#' @return A character vector.
#' @export
removeaccents <- function(x) iconv(x, from = "", to = "ASCII//TRANSLIT")
#' Normalize Unicode text
#'
#' @param x An atomic vector.
#'
#' @return A character vector (currently returned unchanged).
#' @export
normalizeunicode <- function(x) x
#' Normalize text encoding to UTF-8
#'
#' @param x An atomic vector.
#'
#' @return A character vector.
#' @export
normalizeencoding <- function(x) enc2utf8(x)
#' Transliterate text to ASCII
#'
#' @param x An atomic vector.
#'
#' @return A character vector.
#' @export
transliterate <- function(x) iconv(x, from = "", to = "ASCII//TRANSLIT")
#' Pairwise string distances
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#'
#' @return A numeric distance matrix (see [utils::adist()]).
#' @export
textdist <- function(x, y) utils::adist(x, y)
#' Nearest string match
#'
#' @param x An atomic vector.
#' @param choices Candidate strings to match against.
#'
#' @return A character vector of nearest matches.
#' @export
nearesttext <- function(x, choices) choices[max.col(-textdist(x, choices), ties.method = "first")]
#' Nearest string match (alias)
#'
#' @param x An atomic vector.
#' @param choices Candidate strings to match against.
#'
#' @return See [nearesttext()].
#' @export
similartext <- function(x, choices) nearesttext(x, choices)
#' Test for alphabetic-only strings
#'
#' @param x An atomic vector.
#'
#' @return A logical vector.
#' @export
isalpha <- function(x) grepl("^[[:alpha:]]+$", x)
#' Test for alphanumeric-only strings
#'
#' @param x An atomic vector.
#'
#' @return A logical vector.
#' @export
isalphanumeric <- function(x) grepl("^[[:alnum:]]+$", x)
#' Test whether text looks numeric
#'
#' @param x An atomic vector.
#'
#' @return A logical vector.
#' @export
isnumerictext <- function(x) grepl("^[+-]?([0-9]*[.])?[0-9]+$", x)
#' Test whether text looks like an integer
#'
#' @param x An atomic vector.
#'
#' @return A logical vector.
#' @export
isintegertext <- function(x) grepl("^[+-]?[0-9]+$", x)
#' Test whether text looks like an email address
#'
#' @param x An atomic vector.
#'
#' @return A logical vector.
#' @export
isemail <- function(x) grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", x)
#' Test whether text looks like a URL
#'
#' @param x An atomic vector.
#'
#' @return A logical vector.
#' @export
isurl <- function(x) grepl("^(https?|ftp)://", x)
#' Recode values by lookup (alias)
#'
#' @param x An atomic vector.
#' @param old Baseline data.frame or data.table ("before" state).
#' @param new Updated data.frame or data.table ("after" state), or replacement values when used for value substitution.
#'
#' @return See [replacevalues()].
#' @export
recode <- function(x, old, new) replacevalues(x, old, new)
#' Collapse values into named groups
#'
#' @param x An atomic vector.
#' @param groups Named list mapping a replacement label to the values it should collapse.
#'
#' @return `x` with values replaced by their group label.
#' @export
collapsevalues <- function(x, groups) {
  out <- x
  for (nm in names(groups)) out[x %in% groups[[nm]]] <- nm
  out
}
#' Rows where parsing failed
#'
#' @param x An atomic vector.
#' @param fun Function applied to each element, column, or group.
#' @param ... Additional arguments (unused, or passed through depending on the function).
#'
#' @return A tibble of the failed indices and values.
#' @export
parsefailures <- function(x, fun, ...) {
  parsed <- tryCatch(fun(x, ...), error = function(e) rep(NA, length(x)))
  failed <- is.na(parsed) & !is.na(x)
  bt_as_tibble(data.frame(index = which(failed), value = x[failed], stringsAsFactors = FALSE))
}
#' Parse text as integers
#'
#' @param x An atomic vector.
#' @param na Values treated as missing before parsing.
#' @param strict Raise an error if any non-missing value fails to parse.
#'
#' @return An integer vector.
#' @export
parseint <- function(x, na = character(), strict = FALSE) {
  x[x %in% na] <- NA
  out <- suppressWarnings(as.integer(x))
  if (strict && any(!is.na(x) & is.na(out))) stop("Parse failure.", call. = FALSE)
  out
}
#' Parse text as numbers
#'
#' @param x An atomic vector.
#' @param decimal Decimal mark used in the input strings.
#' @param grouping Thousands-grouping mark used in the input strings.
#' @param na Values treated as missing before parsing.
#' @param strict Raise an error if any non-missing value fails to parse.
#'
#' @return A numeric vector.
#' @export
parsenum <- function(x, decimal = ".", grouping = ",", na = character(), strict = FALSE) {
  x[x %in% na] <- NA
  x <- gsub(grouping, "", x, fixed = TRUE)
  if (decimal != ".") x <- sub(decimal, ".", x, fixed = TRUE)
  out <- suppressWarnings(as.numeric(x))
  if (strict && any(!is.na(x) & is.na(out))) stop("Parse failure.", call. = FALSE)
  out
}
#' Parse text as logicals
#'
#' @param x An atomic vector.
#' @param na Values treated as missing before parsing.
#' @param strict Raise an error if any non-missing value fails to parse.
#'
#' @return A logical vector.
#' @export
parselogical <- function(x, na = character(), strict = FALSE) {
  x[x %in% na] <- NA
  map <- c("true" = TRUE, "t" = TRUE, "1" = TRUE, "false" = FALSE, "f" = FALSE, "0" = FALSE)
  out <- unname(map[tolower(x)])
  if (strict && any(!is.na(x) & is.na(out))) stop("Parse failure.", call. = FALSE)
  out
}
#' Parse text as dates, trying multiple formats
#'
#' @param x An atomic vector.
#' @param formats Candidate format strings tried in order.
#' @param tz Time zone used when parsing.
#' @param strict Raise an error if any non-missing value fails to parse.
#'
#' @return A Date vector.
#' @export
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
#' Parse text as date-times, trying multiple formats
#'
#' @param x An atomic vector.
#' @param formats Candidate format strings tried in order.
#' @param tz Time zone used when parsing.
#' @param strict Raise an error if any non-missing value fails to parse.
#'
#' @return A POSIXct vector.
#' @export
parsedatetime <- function(x, formats, tz = "UTC", strict = FALSE) {
  out <- rep(as.POSIXct(NA, tz = tz), length(x))
  for (fmt in formats) {
    idx <- is.na(out) & !is.na(x)
    if (!any(idx)) break
    parsed <- as.POSIXct(x[idx], format = fmt, tz = tz)
    out[idx] <- ifelse(is.na(parsed), out[idx], parsed)
  }
  if (strict && any(!is.na(x) & is.na(out))) stop("Parse failure.", call. = FALSE)
  out
}
#' Parse a percentage string as a number
#'
#' @param x An atomic vector.
#' @param strict Raise an error if any non-missing value fails to parse.
#'
#' @return A numeric vector.
#' @export
parsepercent <- function(x, strict = FALSE) parsenum(sub("%$", "", x), strict = strict) / 100
#' Parse a currency string as a number
#'
#' @param x An atomic vector.
#' @param strict Raise an error if any non-missing value fails to parse.
#'
#' @return A numeric vector.
#' @export
parsecurrency <- function(x, strict = FALSE) parsenum(gsub("[$,]", "", x), strict = strict)
#' Convert selected columns with a function
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#' @param fun Function applied to each element, column, or group.
#' @param ... Additional arguments (unused, or passed through depending on the function).
#'
#' @return `data` with the selected columns converted.
#' @export
convertcols <- function(data, cols, fun, ...) applycols(data, cols = cols, fun = function(x, ...) fun(x, ...), ...)
#' Collapse factor levels into named groups
#'
#' @param x An atomic vector.
#' @param groups Named list mapping a replacement label to the values it should collapse.
#'
#' @return A character vector with levels collapsed.
#' @export
collapselevels <- function(x, groups) collapsevalues(as.character(x), groups)
#' Lump infrequent values into "Other"
#'
#' @param x An atomic vector.
#' @param n Integer count.
#' @param other Label used for values outside the top `n`.
#'
#' @return A character vector.
#' @export
lump <- function(x, n = 5, other = "Other") {
  tab <- sort(table(x), decreasing = TRUE)
  keep <- names(tab)[seq_len(min(n, length(tab)))]
  out <- as.character(x)
  out[!out %in% keep] <- other
  out
}
#' Reorder factor levels
#'
#' @param x An atomic vector.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return A factor with reordered levels.
#' @export
reorderlevels <- function(x, by) factor(x, levels = by)
#' Add levels to a factor
#'
#' @param x An atomic vector.
#' @param levels Character vector of factor levels.
#'
#' @return A factor including the additional levels.
#' @export
expandlevels <- function(x, levels) factor(x, levels = unique(c(base::levels(x), levels)))
nalevel <- function(x, value = "Missing") ifelse(is.na(x), value, as.character(x))
#' Extract the year
#'
#' @param x An atomic vector.
#'
#' @return An integer vector.
#' @export
year <- function(x) as.integer(format(as.Date(x), "%Y"))
#' Extract the month
#'
#' @param x An atomic vector.
#'
#' @return An integer vector.
#' @export
month <- function(x) as.integer(format(as.Date(x), "%m"))
#' Extract the day of month
#'
#' @param x An atomic vector.
#'
#' @return An integer vector.
#' @export
day <- function(x) as.integer(format(as.Date(x), "%d"))
#' Extract the weekday name
#'
#' @param x An atomic vector.
#'
#' @return A character vector.
#' @export
weekday <- function(x) weekdays(as.Date(x))
#' Extract the day of year
#'
#' @param x An atomic vector.
#'
#' @return An integer vector.
#' @export
yearday <- function(x) as.integer(format(as.Date(x), "%j"))
#' Extract the week of year
#'
#' @param x An atomic vector.
#'
#' @return An integer vector.
#' @export
week <- function(x) as.integer(format(as.Date(x), "%U"))
#' Extract the calendar quarter
#'
#' @param x An atomic vector.
#'
#' @return An integer vector.
#' @export
quarter <- function(x) (month(x) - 1L) %/% 3L + 1L
#' Extract the hour
#'
#' @param x An atomic vector.
#'
#' @return An integer vector.
#' @export
hour <- function(x) as.integer(format(as.POSIXct(x), "%H"))
#' Extract the minute
#'
#' @param x An atomic vector.
#'
#' @return An integer vector.
#' @export
minute <- function(x) as.integer(format(as.POSIXct(x), "%M"))
#' Extract the second
#'
#' @param x An atomic vector.
#'
#' @return An integer vector.
#' @export
second <- function(x) as.integer(format(as.POSIXct(x), "%S"))
bt_floor_date <- function(d, unit) {
  switch(
    unit,
    day = d,
    week = d - (as.integer(format(d, "%u")) - 1L),
    month = as.Date(format(d, "%Y-%m-01")),
    year = as.Date(format(d, "%Y-01-01"))
  )
}

bt_ceiling_date <- function(d, unit) {
  switch(
    unit,
    day = d,
    week = bt_floor_date(d, "week") + 7L,
    month = {
      first_of_month <- bt_floor_date(d, "month")
      out <- vapply(first_of_month, function(fd) {
        if (is.na(fd)) return(NA_real_)
        as.numeric(seq.Date(as.Date(fd, origin = "1970-01-01"), by = "1 month", length.out = 2L)[2L])
      }, numeric(1))
      as.Date(out, origin = "1970-01-01")
    },
    year = as.Date(paste0(as.integer(format(d, "%Y")) + 1L, "-01-01"))
  )
}

bt_add_months_one <- function(day, n_months, invalid) {
  if (is.na(day)) {
    return(NA_real_)
  }
  y <- as.integer(format(day, "%Y"))
  m <- as.integer(format(day, "%m"))
  dom <- as.integer(format(day, "%d"))
  total <- (y * 12L + (m - 1L)) + n_months
  ty <- total %/% 12L
  tm <- total %% 12L + 1L
  first_of_target <- as.Date(sprintf("%d-%02d-01", ty, tm))
  days_in_target <- as.integer(format(seq.Date(first_of_target, by = "1 month", length.out = 2L)[2L] - 1L, "%d"))

  if (dom <= days_in_target) {
    return(as.numeric(first_of_target + dom - 1L))
  }

  switch(
    invalid,
    previous = as.numeric(first_of_target + days_in_target - 1L),
    `next` = as.numeric(first_of_target + dom - 1L),
    missing = NA_real_,
    error = stop(sprintf("Adding %d month(s) to %s produces an invalid date.", n_months, day), call. = FALSE)
  )
}

bt_as_datetime <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  as.POSIXct(as.Date(x))
}

#' Round a date down to a unit
#'
#' @param x An atomic vector.
#' @param unit Time unit to round to.
#'
#' @return A Date vector.
#' @export
floordate <- function(x, unit = c("day", "week", "month", "year")) {
  bt_floor_date(as.Date(x), match.arg(unit))
}
#' Round a date up to a unit
#'
#' @param x An atomic vector.
#' @param unit Time unit to round to.
#'
#' @return A Date vector.
#' @export
ceilingdate <- function(x, unit = c("day", "week", "month", "year")) {
  bt_ceiling_date(as.Date(x), match.arg(unit))
}
#' Round a date to the nearest unit
#'
#' @param x An atomic vector.
#' @param unit Time unit to round to.
#'
#' @return A Date vector.
#' @export
rounddate <- function(x, unit = c("day", "week", "month", "year")) {
  unit <- match.arg(unit)
  d <- as.Date(x)
  fl <- bt_floor_date(d, unit)
  ce <- bt_ceiling_date(d, unit)
  closer_to_floor <- (as.numeric(d) - as.numeric(fl)) <= (as.numeric(ce) - as.numeric(d))
  as.Date(ifelse(closer_to_floor, as.numeric(fl), as.numeric(ce)), origin = "1970-01-01")
}
#' Add days to a date
#'
#' @param x An atomic vector.
#' @param n Integer count.
#'
#' @return A Date vector.
#' @export
adddays <- function(x, n) as.Date(x) + n
#' Add weeks to a date
#'
#' @param x An atomic vector.
#' @param n Integer count.
#'
#' @return A Date vector.
#' @export
addweeks <- function(x, n) as.Date(x) + 7 * n
#' Add months to a date
#'
#' @param x An atomic vector.
#' @param n Integer count.
#' @param invalid How to handle an invalid resulting date.
#'
#' @return A Date vector.
#' @export
addmonths <- function(x, n, invalid = c("previous", "next", "missing", "error")) {
  invalid <- match.arg(invalid)
  d <- as.Date(x)
  n <- rep_len(as.integer(n), length(d))
  out <- vapply(seq_along(d), function(i) bt_add_months_one(d[i], n[i], invalid), numeric(1))
  as.Date(out, origin = "1970-01-01")
}
#' Add years to a date
#'
#' @param x An atomic vector.
#' @param n Integer count.
#' @param invalid How to handle an invalid resulting date.
#'
#' @return A Date vector.
#' @export
addyears <- function(x, n, invalid = c("previous", "next", "missing", "error")) {
  addmonths(x, as.integer(n) * 12L, invalid = match.arg(invalid))
}
#' Difference between two dates
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#' @param units Unit used to express the difference.
#'
#' @return A numeric vector of differences.
#' @export
datediff <- function(x, y, units = "days") {
  as.numeric(difftime(bt_as_datetime(x), bt_as_datetime(y), units = units))
}
#' Sequence of dates
#'
#' @param from Start date.
#' @param to End date, or target range for rescaling.
#' @param by Character vector of column names identifying groups or join keys.
#' @param length.out Desired sequence length.
#'
#' @return A Date vector.
#' @export
dateseq <- function(from, to, by = "day", length.out = NULL) {
  if (!is.null(length.out)) {
    return(seq.Date(as.Date(from), as.Date(to), length.out = length.out))
  }
  seq.Date(as.Date(from), as.Date(to), by = by)
}
#' Test whether a date falls within a range
#'
#' @param x An atomic vector.
#' @param start Start bound (inclusive).
#' @param end End bound (inclusive).
#'
#' @return A logical vector.
#' @export
betweendates <- function(x, start, end) as.Date(x) >= as.Date(start) & as.Date(x) <= as.Date(end)
#' Rescale a vector to a new range
#'
#' @param x An atomic vector.
#' @param to End date, or target range for rescaling.
#'
#' @return A rescaled numeric vector.
#' @export
rescale <- function(x, to = c(0, 1)) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1L]) / diff(rng) * diff(to) + to[1L]
}
#' Standardize a vector to mean 0, SD 1
#'
#' @param x An atomic vector.
#'
#' @return A standardized numeric vector.
#' @export
standardize <- function(x) (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
#' Center a vector at its mean
#'
#' @param x An atomic vector.
#'
#' @return A centered numeric vector.
#' @export
center <- function(x) x - mean(x, na.rm = TRUE)
clamp <- function(x, lower = -Inf, upper = Inf) pmin(pmax(x, lower), upper)
#' Winsorize a vector at given quantiles
#'
#' @param x An atomic vector.
#' @param probs Quantile probabilities.
#'
#' @return A winsorized numeric vector.
#' @export
winsorize <- function(x, probs = c(0.01, 0.99)) {
  qs <- stats::quantile(x, probs = probs, na.rm = TRUE)
  clamp(x, qs[[1L]], qs[[2L]])
}
#' Bin a vector into quantile groups
#'
#' @param x An atomic vector.
#' @param n Integer count.
#'
#' @return A factor of quantile bins.
#' @export
quantilegroup <- function(x, n = 5) cut(x, breaks = stats::quantile(x, probs = seq(0, 1, length.out = n + 1L), na.rm = TRUE), include.lowest = TRUE)
#' Period-over-period percent change
#'
#' @param x An atomic vector.
#'
#' @return A numeric vector of percent changes.
#' @export
percentchange <- function(x) c(NA, diff(x) / utils::head(x, -1L) * 100)
#' Assert that required column names are present
#'
#' @param data A data.frame or data.table.
#' @param names Required column names.
#'
#' @return `data`, invisibly, if the assertion passes.
#' @export
assertnames <- function(data, names) {
  if (!all(names %in% base::names(bt_as_data_frame(data)))) stop("Missing required names.", call. = FALSE)
  invisible(data)
}
#' Assert that columns exist (alias)
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#'
#' @return See [assert_cols()].
#' @export
assertcols <- function(data, cols) assert_cols(data, cols)
#' Assert that a key is unique (alias)
#'
#' @param data A data.frame or data.table.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return See [assert_key()].
#' @export
assertkey <- function(data, by) assert_key(data, by)
#' Assert that a row-wise condition holds for every row
#'
#' @param data A data.frame or data.table.
#' @param condition A logical expression evaluated in the context of `data`.
#'
#' @return `data`, invisibly, if the assertion passes.
#' @export
assertrows <- function(data, condition) {
  df <- bt_as_data_frame(data)
  cond <- bt_eval_logical(substitute(condition), df, nrow(df))
  if (!all(cond)) stop("Some rows failed the assertion.", call. = FALSE)
  invisible(data)
}
#' Assert that a column only contains allowed values
#'
#' @param data A data.frame or data.table.
#' @param column Name of a single column.
#' @param allowed Vector of permitted values.
#'
#' @return `data`, invisibly, if the assertion passes.
#' @export
assertvalues <- function(data, column, allowed) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  if (length(column) != 1L) stop("`column` must be one column.", call. = FALSE)
  bad <- !df[[column]] %in% allowed & !is.na(df[[column]])
  if (any(bad)) stop("Some values are not allowed.", call. = FALSE)
  invisible(data)
}
#' Assert that a column falls within a numeric range
#'
#' @param data A data.frame or data.table.
#' @param column Name of a single column.
#' @param lower Lower bound.
#' @param upper Upper bound.
#'
#' @return `data`, invisibly, if the assertion passes.
#' @export
assertrange <- function(data, column, lower, upper) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  x <- df[[column]]
  if (any(!is.na(x) & (x < lower | x > upper))) stop("Values out of range.", call. = FALSE)
  invisible(data)
}
#' Assert that a column has an expected class
#'
#' @param data A data.frame or data.table.
#' @param column Name of a single column.
#' @param class Class name to check against.
#'
#' @return `data`, invisibly, if the assertion passes.
#' @export
asserttype <- function(data, column, class) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  if (!inherits(df[[column]], class)) stop("Unexpected type.", call. = FALSE)
  invisible(data)
}
#' Assert that selected columns are unique
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#'
#' @return `data`, invisibly, if the assertion passes.
#' @export
assertunique <- function(data, cols) {
  if (anyDuplicated(bt_as_data_frame(data)[, bt_resolve_cols(bt_as_data_frame(data), cols), drop = FALSE])) stop("Values are not unique.", call. = FALSE)
  invisible(data)
}
#' Assert that there are no missing values
#'
#' @param data A data.frame or data.table.
#' @param cols Character vector of column names.
#'
#' @return `data`, invisibly, if the assertion passes.
#' @export
assertcomplete <- function(data, cols = NULL) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) df <- df[, bt_resolve_cols(df, cols), drop = FALSE]
  if (any(!stats::complete.cases(df))) stop("Missing values found.", call. = FALSE)
  invisible(data)
}
#' Rows that fail a condition
#'
#' @param data A data.frame or data.table.
#' @param condition A logical expression evaluated in the context of `data`.
#'
#' @return The rows of `data` for which `condition` is not `TRUE`.
#' @export
invalidrows <- function(data, condition) {
  df <- bt_as_data_frame(data)
  cond <- bt_eval_logical(substitute(condition), df, nrow(df))
  bt_as_tibble(df[!cond, , drop = FALSE])
}
#' Rows whose value is not in the allowed set
#'
#' @param data A data.frame or data.table.
#' @param column Name of a single column.
#' @param allowed Vector of permitted values.
#'
#' @return The rows of `data` with disallowed values.
#' @export
invalidvalues <- function(data, column, allowed) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  bt_as_tibble(df[!df[[column]] %in% allowed & !is.na(df[[column]]), , drop = FALSE])
}
#' Rows whose value falls outside a range
#'
#' @param data A data.frame or data.table.
#' @param column Name of a single column.
#' @param lower Lower bound.
#' @param upper Upper bound.
#'
#' @return The rows of `data` outside `[lower, upper]`.
#' @export
outofrange <- function(data, column, lower, upper) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  bt_as_tibble(df[!is.na(df[[column]]) & (df[[column]] < lower | df[[column]] > upper), , drop = FALSE])
}
#' Compare two tables for equality
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#' @param ignoreorder Ignore row order when comparing.
#' @param ignorerownames Ignore row names when comparing.
#' @param tolerance Numeric join/comparison tolerance.
#'
#' @return A single logical value.
#' @export
equaldata <- function(x, y, ignoreorder = FALSE, ignorerownames = TRUE, tolerance = sqrt(.Machine$double.eps)) {
  x_df <- bt_as_data_frame(x); y_df <- bt_as_data_frame(y)
  if (ignoreorder) {
    x_df <- x_df[do.call(order, x_df), , drop = FALSE]
    y_df <- y_df[do.call(order, y_df), , drop = FALSE]
  }
  isTRUE(all.equal(x_df, y_df, tolerance = tolerance, check.attributes = !ignorerownames))
}
#' Test whether two tables share the same column names
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#'
#' @return A single logical value.
#' @export
sameschema <- function(x, y) identical(names(bt_as_data_frame(x)), names(bt_as_data_frame(y)))
#' Compare the schemas of two tables
#'
#' @param x An atomic vector.
#' @param y An atomic vector, data.frame, or data.table, depending on the function.
#'
#' @return A tibble describing shared and differing columns/types.
#' @export
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
#' Rows present in `new` but not in `old`
#'
#' @param old Baseline data.frame or data.table ("before" state).
#' @param new Updated data.frame or data.table ("after" state), or replacement values when used for value substitution.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return The added rows.
#' @export
addedrows <- function(old, new, by = NULL) {
  old_df <- bt_as_data_frame(old); new_df <- bt_as_data_frame(new)
  if (is.null(by)) return(bt_as_tibble(setdiff(new_df, old_df)))
  by <- bt_resolve_cols(old_df, by); bt_resolve_cols(new_df, by)
  old_dt <- bt_as_data_table(old_df); new_dt <- bt_as_data_table(new_df)
  old_keys <- unique(old_dt[, by, with = FALSE])
  bt_as_tibble(new_dt[!old_keys, on = by])
}
#' Rows present in `old` but not in `new`
#'
#' @param old Baseline data.frame or data.table ("before" state).
#' @param new Updated data.frame or data.table ("after" state), or replacement values when used for value substitution.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return The removed rows.
#' @export
removedrows <- function(old, new, by = NULL) addedrows(new, old, by = by)
#' Rows whose key appears in both tables
#'
#' @param old Baseline data.frame or data.table ("before" state).
#' @param new Updated data.frame or data.table ("after" state), or replacement values when used for value substitution.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return The matched rows from both tables, suffixed `.old`/`.new`.
#' @export
changedrows <- function(old, new, by = NULL) {
  old_df <- bt_as_data_frame(old); new_df <- bt_as_data_frame(new)
  if (is.null(by)) return(bt_as_tibble(setdiff(rbind(old_df, new_df), intersect(old_df, new_df))))
  by <- bt_resolve_cols(old_df, by); bt_resolve_cols(new_df, by)
  bt_as_tibble(merge(old_df, new_df, by = by, suffixes = c(".old", ".new"), all = FALSE))
}
#' Compare the columns of two tables
#'
#' @param old Baseline data.frame or data.table ("before" state).
#' @param new Updated data.frame or data.table ("after" state), or replacement values when used for value substitution.
#' @param by Character vector of column names identifying groups or join keys.
#'
#' @return See [compareschema()].
#' @export
changedcols <- function(old, new, by = NULL) compareschema(old, new)
