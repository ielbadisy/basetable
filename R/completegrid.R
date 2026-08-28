completegrid <- function(data, cols, fill = list()) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)

  if (length(cols) < 1L) {
    stop("`cols` must contain at least one column.", call. = FALSE)
  }
  if (!is.list(fill)) {
    stop("`fill` must be a list.", call. = FALSE)
  }

  values <- lapply(df[cols], unique)
  grid <- do.call(expand.grid, c(values, list(stringsAsFactors = FALSE)))
  names(grid) <- cols

  out <- bt_join_rows(grid, df, by = cols, all.x = TRUE)

  if (length(fill) > 0L) {
    for (nm in names(fill)) {
      if (nm %in% names(out)) {
        missing <- is.na(out[[nm]])
        if (any(missing)) {
          out[[nm]][missing] <- fill[[nm]]
        }
      }
    }
  }

  bt_as_data_table(out)
}
