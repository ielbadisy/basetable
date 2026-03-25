assert_cols <- function(data, cols) {
  bt_resolve_cols(bt_as_data_frame(data), cols)
  invisible(data)
}

common_names <- function(x, y) {
  intersect(names(bt_as_data_frame(x)), names(bt_as_data_frame(y)))
}

duplicated_keys <- function(data, by) {
  df <- bt_as_data_frame(data)
  by <- bt_resolve_cols(df, by)
  key <- interaction(df[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  counts <- as.data.frame(table(key), stringsAsFactors = FALSE)
  counts <- counts[counts$Freq > 1L, , drop = FALSE]

  if (nrow(counts) == 0L) {
    return(df[FALSE, by, drop = FALSE])
  }

  rows <- match(as.character(counts$key), as.character(key))
  out <- cbind(df[rows, by, drop = FALSE], N = counts$Freq)
  rownames(out) <- NULL
  out
}

assert_key <- function(data, by) {
  dup <- duplicated_keys(data, by)
  if (nrow(dup) > 0L) {
    stop("`by` does not identify unique rows.", call. = FALSE)
  }
  invisible(data)
}
