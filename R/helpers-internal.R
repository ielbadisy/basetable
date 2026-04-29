bt_as_data_table <- function(data) {
  if (!inherits(data, c("data.frame", "data.table"))) {
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
