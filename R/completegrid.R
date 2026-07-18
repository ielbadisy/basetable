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
  grid <- do.call(data.table::CJ, c(values, list(sorted = FALSE)))
  names(grid) <- cols

  out <- data.table::merge.data.table(grid, df, by = cols, all.x = TRUE, sort = FALSE)

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

  bt_as_tibble(out)
}
