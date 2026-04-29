#' Filter rows
#'
#' Keep rows where all supplied logical expressions evaluate to `TRUE`.
#'
#' @param data A data frame or data.table.
#' @param ... Logical expressions evaluated in `data`.
#'
#' @return A tibble.
#' @export
filter <- function(data, ...) {
  df <- bt_as_data_frame(data)
  dots <- as.list(substitute(list(...)))[-1L]

  if (length(dots) == 0L) {
    return(bt_as_tibble(df))
  }

  env <- list2env(as.list(df), parent = parent.frame())
  keep <- rep(TRUE, nrow(df))
  for (expr in dots) {
    value <- eval(expr, envir = env, enclos = parent.frame())
    if (!is.logical(value) || length(value) != nrow(df)) {
      stop("Each filter expression must evaluate to a logical vector with one value per row.", call. = FALSE)
    }
    value[is.na(value)] <- FALSE
    keep <- keep & value
  }

  bt_as_tibble(df[keep, , drop = FALSE])
}

#' Select columns
#'
#' Select columns by name.
#'
#' @inheritParams filter
#' @param cols Character vector of column names.
#'
#' @return A tibble.
#' @export
select <- function(data, cols) {
  pick(data, cols)
}

#' Rename columns
#'
#' Rename columns using `new = old` pairs. Old column names may be supplied as
#' bare names or character strings.
#'
#' @inheritParams filter
#' @param ... Named rename expressions.
#'
#' @return A tibble.
#' @export
rename <- function(data, ...) {
  df <- bt_as_data_frame(data)
  dots <- as.list(substitute(list(...)))[-1L]

  if (length(dots) == 0L) {
    return(bt_as_tibble(df))
  }

  new_names <- names(dots)
  if (is.null(new_names) || any(!nzchar(new_names))) {
    stop("All rename expressions must be named as `new = old`.", call. = FALSE)
  }

  old_names <- vapply(dots, bt_rename_old_name, character(1), enclos = parent.frame())
  old_names <- bt_resolve_cols(df, old_names)
  names(df)[match(old_names, names(df))] <- new_names
  bt_as_tibble(df)
}

#' Arrange rows
#'
#' Sort rows by one or more columns.
#'
#' @inheritParams filter
#' @param by Character vector of sort columns.
#' @param decreasing Logical scalar or vector. Use descending order for each
#'   matching column.
#' @param na.last Place missing values last.
#'
#' @return A tibble.
#' @export
arrange <- function(data, by, decreasing = FALSE, na.last = TRUE) {
  reorder(data, by = by, decreasing = decreasing, na.last = na.last)
}

#' Mutate columns
#'
#' Add or replace columns using expressions evaluated in `data`.
#'
#' @inheritParams filter
#' @param .keep Keep existing columns.
#'
#' @return A tibble.
#' @export
mutate <- function(data, ..., .keep = TRUE) {
  transform(data, ..., .keep = .keep)
}

#' Transmute columns
#'
#' Create columns and keep only the new variables.
#'
#' @inheritParams mutate
#'
#' @return A tibble.
#' @export
transmute <- function(data, ...) {
  transform(data, ..., .keep = FALSE)
}

#' Summarise columns
#'
#' Create one-row summaries, optionally by group.
#'
#' @inheritParams filter
#' @param ... Named summary expressions.
#' @param by Optional character vector of grouping columns.
#'
#' @return A tibble.
#' @export
summarise <- function(data, ..., by = NULL) {
  df <- bt_as_data_frame(data)
  dots <- as.list(substitute(list(...)))[-1L]

  if (length(dots) == 0L) {
    stop("At least one summary expression is required.", call. = FALSE)
  }

  summary_names <- names(dots)
  if (is.null(summary_names) || any(!nzchar(summary_names))) {
    stop("All summary expressions must be named.", call. = FALSE)
  }

  by <- if (is.null(by)) character(0) else bt_resolve_cols(df, by)
  pieces <- if (length(by) == 0L) {
    list(df)
  } else {
    split(df, by = by, keep.by = TRUE)
  }

  rows <- lapply(pieces, function(piece) {
    env <- list2env(as.list(piece), parent = parent.frame())
    values <- lapply(dots, function(expr) eval(expr, envir = env, enclos = parent.frame()))
    bad <- vapply(values, function(x) length(x) != 1L, logical(1))
    if (any(bad)) {
      stop("Each summary expression must return exactly one value per group.", call. = FALSE)
    }
    out <- as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
    names(out) <- summary_names
    if (length(by) > 0L) {
      out <- cbind(piece[1L, by, drop = FALSE], out, stringsAsFactors = FALSE)
    }
    out
  })

  bt_as_tibble(data.table::rbindlist(rows, fill = TRUE))
}

#' @rdname summarise
#' @export
summarize <- summarise

#' Distinct rows
#'
#' Return unique rows, optionally considering only selected columns.
#'
#' @inheritParams filter
#' @param cols Optional character vector of columns used to determine
#'   uniqueness.
#' @param .keep_all Keep all columns when `cols` is supplied.
#'
#' @return A tibble.
#' @export
distinct <- function(data, cols = NULL, .keep_all = FALSE) {
  df <- bt_as_data_frame(data)
  if (is.null(cols)) {
    return(bt_as_tibble(unique(df)))
  }

  cols <- bt_resolve_cols(df, cols)
  key <- !duplicated(df[, cols, drop = FALSE])
  out <- if (.keep_all) df[key, , drop = FALSE] else df[key, cols, drop = FALSE]
  bt_as_tibble(out)
}

#' Slice rows
#'
#' Select rows by integer position.
#'
#' @inheritParams filter
#' @param rows Integer row positions.
#'
#' @return A tibble.
#' @export
slice <- function(data, rows) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(df[rows, , drop = FALSE])
}

#' Relocate columns
#'
#' Move selected columns before or after another column.
#'
#' @inheritParams select
#' @param .before,.after Optional single column name controlling placement.
#'
#' @return A tibble.
#' @export
relocate <- function(data, cols, .before = NULL, .after = NULL) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)

  if (!is.null(.before) && !is.null(.after)) {
    stop("Use only one of `.before` or `.after`.", call. = FALSE)
  }

  remaining <- setdiff(names(df), cols)
  if (is.null(.before) && is.null(.after)) {
    order <- c(cols, remaining)
  } else if (!is.null(.before)) {
    .before <- bt_resolve_cols(df, .before)
    if (length(.before) != 1L) {
      stop("`.before` must name exactly one column.", call. = FALSE)
    }
    pos <- match(.before, remaining)
    order <- append(remaining, cols, after = pos - 1L)
  } else {
    .after <- bt_resolve_cols(df, .after)
    if (length(.after) != 1L) {
      stop("`.after` must name exactly one column.", call. = FALSE)
    }
    pos <- match(.after, remaining)
    order <- append(remaining, cols, after = pos)
  }

  bt_as_tibble(df[, order, drop = FALSE])
}

#' Bind rows
#'
#' Combine data frames by rows.
#'
#' @param ... Data frames or a single list of data frames.
#' @param id Optional name for a source identifier column.
#'
#' @return A tibble.
#' @export
bind_rows <- function(..., id = NULL) {
  dots <- list(...)
  if (length(dots) == 1L && is.list(dots[[1L]]) && !inherits(dots[[1L]], "data.frame")) {
    dots <- dots[[1L]]
  }
  combine(dots, id = id)
}

#' Bind columns
#'
#' Combine data frames by columns.
#'
#' @param ... Data frames.
#'
#' @return A tibble.
#' @export
bind_cols <- function(...) {
  dots <- lapply(list(...), bt_as_data_frame)
  if (length(dots) == 0L) {
    return(tibble::tibble())
  }
  n <- vapply(dots, nrow, integer(1))
  if (length(unique(n)) > 1L) {
    stop("All inputs must have the same number of rows.", call. = FALSE)
  }
  bt_as_tibble(do.call(cbind, c(dots, stringsAsFactors = FALSE)))
}

bt_rename_old_name <- function(expr, enclos) {
  if (is.symbol(expr)) {
    return(as.character(expr))
  }
  value <- eval(expr, envir = parent.frame(), enclos = enclos)
  if (!is.character(value) || length(value) != 1L) {
    stop("Rename targets must be bare column names or single strings.", call. = FALSE)
  }
  value
}
