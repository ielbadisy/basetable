dims <- function(data) {
  df <- bt_as_data_frame(data)
  data.frame(rows = nrow(df), cols = ncol(df), stringsAsFactors = FALSE)
}

types <- function(data) {
  df <- bt_as_data_frame(data)
  data.frame(
    column = names(df),
    class = vapply(df, bt_mode, character(1)),
    typeof = vapply(df, typeof, character(1)),
    stringsAsFactors = FALSE
  )
}

headtail <- function(data, n = 3) {
  df <- bt_as_data_frame(data)
  if (nrow(df) <= (2L * n)) {
    return(df)
  }
  idx <- c(seq_len(n), seq.int(nrow(df) - n + 1L, nrow(df)))
  df[idx, , drop = FALSE]
}

glimpse <- function(data, width = getOption("width")) {
  df <- bt_as_data_frame(data)
  cat(sprintf("Rows: %s  Columns: %s\n", nrow(df), ncol(df)))
  for (nm in names(df)) {
    vals <- paste(utils::head(as.character(df[[nm]]), 3L), collapse = ", ")
    line <- sprintf("$ %s <%s> %s", nm, bt_mode(df[[nm]]), vals)
    cat(substr(line, 1L, width), "\n", sep = "")
  }
  invisible(data)
}

describe <- function(data, cols = NULL, top_n = 3) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) {
    cols <- bt_resolve_cols(df, cols)
    df <- df[, cols, drop = FALSE]
  }

  rows <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    is_num <- is.numeric(x)
    qs <- if (is_num) stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE) else rep(NA_real_, 3L)

    data.frame(
      column = nm,
      class = bt_mode(x),
      n = length(x),
      missing = sum(is.na(x)),
      missing_prop = mean(is.na(x)),
      distinct = bt_distinct_n(x),
      mean = if (is_num) mean(x, na.rm = TRUE) else NA_real_,
      sd = if (is_num) stats::sd(x, na.rm = TRUE) else NA_real_,
      min = if (is_num) suppressWarnings(min(x, na.rm = TRUE)) else NA_real_,
      q25 = qs[[1L]],
      median = qs[[2L]],
      q75 = qs[[3L]],
      max = if (is_num) suppressWarnings(max(x, na.rm = TRUE)) else NA_real_,
      top = if (!is_num) bt_top_values(x, n = top_n) else "",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

profile <- function(data, cols = NULL, top_n = 3) {
  describe(data, cols = cols, top_n = top_n)
}

freq <- function(data, column, by = NULL, prop = FALSE, sort = TRUE) {
  df <- bt_as_data_frame(data)
  column <- bt_resolve_cols(df, column)
  if (length(column) != 1L) {
    stop("`column` must name exactly one column.", call. = FALSE)
  }

  groups <- if (is.null(by)) column else c(bt_resolve_cols(df, by), column)
  out <- as.data.frame(table(df[, groups, drop = FALSE], useNA = "ifany"), stringsAsFactors = FALSE)
  names(out)[ncol(out)] <- "n"
  out <- out[out$n > 0L, , drop = FALSE]

  if (prop) {
    if (is.null(by)) {
      out$prop <- out$n / sum(out$n)
    } else {
      by <- bt_resolve_cols(df, by)
      totals <- stats::aggregate(out$n, out[by], sum)
      names(totals)[ncol(totals)] <- ".total"
      out <- merge(out, totals, by = by, sort = FALSE)
      out$prop <- out$n / out$.total
      out$.total <- NULL
    }
  }

  if (sort) {
    out <- out[order(out$n, decreasing = TRUE), , drop = FALSE]
    rownames(out) <- NULL
  }

  out
}

compare <- function(x, y, by = NULL) {
  x_df <- bt_as_data_frame(x)
  y_df <- bt_as_data_frame(y)

  out <- list(
    dims = data.frame(
      object = c("x", "y"),
      rows = c(nrow(x_df), nrow(y_df)),
      cols = c(ncol(x_df), ncol(y_df)),
      stringsAsFactors = FALSE
    ),
    names = data.frame(
      column = union(names(x_df), names(y_df)),
      in_x = union(names(x_df), names(y_df)) %in% names(x_df),
      in_y = union(names(x_df), names(y_df)) %in% names(y_df),
      stringsAsFactors = FALSE
    ),
    types = merge(types(x_df), types(y_df), by = "column", all = TRUE, suffixes = c(".x", ".y")),
    missing = merge(
      missingness(x_df, margin = "column"),
      missingness(y_df, margin = "column"),
      by = "column",
      all = TRUE,
      suffixes = c(".x", ".y")
    )
  )

  if (!is.null(by)) {
    by <- bt_resolve_cols(x_df, by)
    bt_resolve_cols(y_df, by)
    x_keys <- unique(x_df[, by, drop = FALSE])
    y_keys <- unique(y_df[, by, drop = FALSE])
    out$key_overlap <- data.frame(
      x_unique = nrow(x_keys),
      y_unique = nrow(y_keys),
      common = nrow(merge(x_keys, y_keys, by = by)),
      stringsAsFactors = FALSE
    )
  }

  out
}
