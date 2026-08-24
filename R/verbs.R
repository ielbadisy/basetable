#' Filter rows
#'
#' Keep rows where all supplied logical expressions evaluate to `TRUE`.
#'
#' @param data A data frame or data.table.
#' @param ... Logical expressions evaluated in `data`.
#'
#' @return A data.table.
#' @export
filter <- function(data, ...) {
  dt <- bt_as_data_table_ro(data)
  dots <- as.list(substitute(list(...)))[-1L]

  if (length(dots) == 0L) {
    return(data.table::copy(dt))
  }

  keep <- rep(TRUE, nrow(dt))
  for (expr in dots) {
    value <- eval(expr, envir = dt, enclos = parent.frame())
    if (!is.logical(value) || length(value) != nrow(dt)) {
      stop("Each filter expression must evaluate to a logical vector with one value per row.", call. = FALSE)
    }
    value[is.na(value)] <- FALSE
    keep <- keep & value
  }

  dt[keep]
}

#' Select columns
#'
#' Select columns by name.
#'
#' @inheritParams filter
#' @param cols Character vector of column names.
#'
#' @return A data.table.
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
#' @return A data.table.
#' @export
rename <- function(data, ...) {
  dt <- bt_as_data_table(data)
  dots <- as.list(substitute(list(...)))[-1L]

  if (length(dots) == 0L) {
    return(dt)
  }

  new_names <- names(dots)
  if (is.null(new_names) || any(!nzchar(new_names))) {
    stop("All rename expressions must be named as `new = old`.", call. = FALSE)
  }

  old_names <- vapply(dots, bt_rename_old_name, character(1), enclos = parent.frame())
  old_names <- bt_resolve_cols(dt, old_names)
  data.table::setnames(dt, old = old_names, new = new_names)
  dt
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
#' @return A data.table.
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
#' @return A data.table.
#' @export
mutate <- function(data, ..., .keep = TRUE) {
  dots <- as.list(substitute(list(...)))[-1L]
  bt_transform(data, dots, env = parent.frame(), keep = .keep)
}

#' Transmute columns
#'
#' Create columns and keep only the new variables.
#'
#' @inheritParams mutate
#'
#' @return A data.table.
#' @export
transmute <- function(data, ...) {
  dots <- as.list(substitute(list(...)))[-1L]
  bt_transform(data, dots, env = parent.frame(), keep = FALSE)
}

#' Summarise columns
#'
#' Create one-row summaries, optionally by group.
#'
#' @inheritParams filter
#' @param ... Named summary expressions.
#' @param by Optional character vector of grouping columns.
#'
#' @return A data.table.
#' @export
summarise <- function(data, ..., by = NULL) {
  dt <- bt_as_data_table_ro(data)
  dots <- as.list(substitute(list(...)))[-1L]

  if (length(dots) == 0L) {
    stop("At least one summary expression is required.", call. = FALSE)
  }

  summary_names <- names(dots)
  if (is.null(summary_names) || any(!nzchar(summary_names))) {
    stop("All summary expressions must be named.", call. = FALSE)
  }

  by <- if (is.null(by)) character(0) else bt_resolve_cols(dt, by)
  j_call <- as.call(c(quote(list), dots))

  out <- if (length(by) == 0L) {
    dt[, eval(j_call)]
  } else {
    dt[, eval(j_call), keyby = by]
  }

  expected_groups <- if (length(by) == 0L) 1L else data.table::uniqueN(dt, by = by)
  if (nrow(out) != expected_groups) {
    stop("Each summary expression must return exactly one value per group.", call. = FALSE)
  }

  bt_as_data_table(out)
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
#' @return A data.table.
#' @export
distinct <- function(data, cols = NULL, .keep_all = FALSE) {
  dt <- bt_as_data_table_ro(data)
  if (is.null(cols)) {
    return(unique(dt))
  }

  cols <- bt_resolve_cols(dt, cols)
  if (.keep_all) {
    unique(dt, by = cols)
  } else {
    unique(dt[, cols, with = FALSE])
  }
}

#' Slice rows
#'
#' Select rows by integer position.
#'
#' @inheritParams filter
#' @param rows Integer row positions.
#'
#' @return A data.table.
#' @export
slice <- function(data, rows) {
  dt <- bt_as_data_table_ro(data)
  dt[rows]
}

#' Relocate columns
#'
#' Move selected columns before or after another column.
#'
#' @inheritParams select
#' @param .before,.after Optional single column name controlling placement.
#'
#' @return A data.table.
#' @export
relocate <- function(data, cols, .before = NULL, .after = NULL) {
  dt <- bt_as_data_table_ro(data)
  cols <- bt_resolve_cols(dt, cols)

  if (!is.null(.before) && !is.null(.after)) {
    stop("Use only one of `.before` or `.after`.", call. = FALSE)
  }

  remaining <- setdiff(names(dt), cols)
  if (is.null(.before) && is.null(.after)) {
    order <- c(cols, remaining)
  } else if (!is.null(.before)) {
    .before <- bt_resolve_cols(dt, .before)
    if (length(.before) != 1L) {
      stop("`.before` must name exactly one column.", call. = FALSE)
    }
    pos <- match(.before, remaining)
    order <- append(remaining, cols, after = pos - 1L)
  } else {
    .after <- bt_resolve_cols(dt, .after)
    if (length(.after) != 1L) {
      stop("`.after` must name exactly one column.", call. = FALSE)
    }
    pos <- match(.after, remaining)
    order <- append(remaining, cols, after = pos)
  }

  dt[, order, with = FALSE]
}

#' Bind rows
#'
#' Combine data frames by rows.
#'
#' @param ... Data frames or a single list of data frames.
#' @param id Optional name for a source identifier column.
#'
#' @return A data.table.
#' @export
bind_rows <- function(..., id = NULL) {
  dots <- list(...)
  if (length(dots) == 1L && is.list(dots[[1L]]) && !inherits(dots[[1L]], "data.frame")) {
    dots <- dots[[1L]]
  }
  data.table::rbindlist(
    lapply(dots, bt_as_data_table_ro),
    use.names = TRUE,
    fill = TRUE,
    idcol = id
  )
}

#' Bind columns
#'
#' Combine data frames by columns.
#'
#' @param ... Data frames.
#'
#' @return A data.table.
#' @export
bind_cols <- function(...) {
  dots <- lapply(list(...), bt_as_data_frame)
  if (length(dots) == 0L) {
    return(data.table::data.table())
  }
  n <- vapply(dots, nrow, integer(1))
  if (length(unique(n)) > 1L) {
    stop("All inputs must have the same number of rows.", call. = FALSE)
  }
  bt_as_data_table(do.call(cbind, c(dots, stringsAsFactors = FALSE)))
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
