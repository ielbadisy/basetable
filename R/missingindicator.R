missingindicator <- function(data, cols = NULL, prefix = "missing_") {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols, allow_null = TRUE)

  if (length(cols) == 0L) {
    cols <- names(df)
  }

  indicator_names <- paste0(prefix, cols)
  if (any(indicator_names %in% names(df))) {
    stop("Indicator columns already exist.", call. = FALSE)
  }

  for (i in seq_along(cols)) {
    df[[indicator_names[[i]]]] <- bt_is_blank(df[[cols[[i]]]])
  }

  bt_as_tibble(df)
}
