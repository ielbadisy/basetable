bt_as_data_table <- function(data) {
  if (!inherits(data, c("data.frame", "data.table"))) {
    stop("`data` must be a data.frame or data.table.", call. = FALSE)
  }

  data.table::as.data.table(data)
}

# Like bt_as_data_table(), but skips the defensive copy when `data` is
# already a data.table. Only safe for callers that never mutate the
# result in place (no `:=`, no `set*()`, no `x[[nm]][...] <-`); such
# callers must use bt_as_data_table() instead.
bt_as_data_table_ro <- function(data) {
  if (data.table::is.data.table(data)) {
    return(data)
  }
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame or data.table.", call. = FALSE)
  }

  data.table::as.data.table(data)
}

bt_as_data_frame <- function(data) {
  as.data.frame(data, stringsAsFactors = FALSE)
}

bt_as_tibble <- function(data) {
  tibble::as_tibble(data)
}

# Vectorized last-observation-carried-forward, works for any atomic vector
# type (unlike data.table::nafill(), which is numeric-only).
bt_locf <- function(x) {
  ok <- !is.na(x)
  idx <- cumsum(ok)
  fill_pos <- which(!ok & idx > 0L)
  if (length(fill_pos) > 0L) {
    x[fill_pos] <- x[ok][idx[fill_pos]]
  }
  x
}

bt_nocb <- function(x) rev(bt_locf(rev(x)))

bt_col_expr <- function(expr, data) {
  eval(expr, envir = data, enclos = parent.frame())
}

bt_resolve_cols <- function(data, cols, allow_null = FALSE) {
  nms <- names(data)

  if (is.null(cols)) {
    if (allow_null) {
      return(nms)
    }
    stop("`cols` must not be NULL.", call. = FALSE)
  }

  if (!is.character(cols)) {
    stop("Column specification must be a character vector.", call. = FALSE)
  }

  missing_cols <- setdiff(cols, nms)
  if (length(missing_cols) > 0L) {
    stop(
      sprintf("Unknown columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  cols
}

bt_recycle_flag <- function(x, n, arg) {
  if (length(x) == 1L) {
    rep(x, n)
  } else if (length(x) == n) {
    x
  } else {
    stop(sprintf("`%s` must have length 1 or %s.", arg, n), call. = FALSE)
  }
}

bt_mode <- function(x) {
  cls <- class(x)
  if (length(cls) == 0L) {
    typeof(x)
  } else {
    cls[[1L]]
  }
}

bt_top_values <- function(x, n = 3L) {
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- "<NA>"
  tab <- sort(table(x_chr), decreasing = TRUE)
  if (length(tab) == 0L) {
    return("")
  }
  top <- utils::head(tab, n)
  paste(sprintf("%s (%s)", names(top), as.integer(top)), collapse = ", ")
}

bt_distinct_n <- function(x) {
  data.table::uniqueN(x, na.rm = FALSE)
}

bt_is_blank <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  is.na(x) | trimws(as.character(x)) == ""
}

bt_clean_names <- function(nms, method = c("unique", "universal", "minimal")) {
  method <- match.arg(method)

  if (method == "minimal") {
    return(nms)
  }

  cleaned <- tolower(nms)
  cleaned <- gsub("[^[:alnum:]]+", "_", cleaned)
  cleaned <- gsub("^_+|_+$", "", cleaned)
  cleaned[cleaned == ""] <- "x"

  if (method == "universal") {
    cleaned <- make.names(cleaned, unique = FALSE)
  }

  make.unique(cleaned, sep = "_")
}

bt_key_expr <- function(data, by) {
  by <- bt_resolve_cols(data, by)
  interaction(data[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
}

bt_order_data <- function(df, by, decreasing = FALSE, na.last = TRUE) {
  by <- bt_resolve_cols(df, by)
  decreasing <- bt_recycle_flag(decreasing, length(by), "decreasing")
  ord <- do.call(order, c(df[by], list(decreasing = decreasing, na.last = na.last)))
  df[ord, , drop = FALSE]
}

bt_eval_in_data <- function(expr, data) {
  eval(expr, envir = as.list(data), enclos = parent.frame())
}

bt_eval_logical <- function(expr, data, n) {
  value <- bt_eval_in_data(expr, data)
  if (!is.logical(value) || length(value) != n) {
    stop("Expression must evaluate to a logical vector with one value per row.", call. = FALSE)
  }
  value[is.na(value)] <- FALSE
  value
}

bt_split_by <- function(data, by, drop = FALSE, keepby = FALSE) {
  df <- bt_as_data_frame(data)
  by <- bt_resolve_cols(df, by)
  key <- interaction(df[, by, drop = FALSE], drop = drop, lex.order = TRUE)
  pieces <- base::split(df, key, drop = drop)

  if (!keepby) {
    pieces <- lapply(pieces, function(piece) bt_as_tibble(piece[, setdiff(names(piece), by), drop = FALSE]))
  } else {
    pieces <- lapply(pieces, bt_as_tibble)
  }

  pieces
}

bt_group_keys <- function(data, by) {
  df <- bt_as_data_frame(data)
  by <- bt_resolve_cols(df, by)
  unique(df[, by, drop = FALSE])
}

bt_set_row_names <- function(x, n) {
  if (length(x) == 1L) {
    rep(x, n)
  } else if (length(x) == n) {
    x
  } else {
    stop("Length mismatch.", call. = FALSE)
  }
}
