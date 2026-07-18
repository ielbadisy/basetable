missingrows <- function(data, cols = NULL, mode = c("any", "all")) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols, allow_null = TRUE)
  mode <- match.arg(mode)

  if (length(cols) == 0L) {
    return(bt_as_tibble(df[0, , drop = FALSE]))
  }

  miss <- do.call(cbind, lapply(df[cols], bt_is_blank))
  if (is.null(dim(miss))) {
    miss <- matrix(miss, ncol = 1L)
  }

  keep <- switch(
    mode,
    any = rowSums(miss) > 0L,
    all = rowSums(miss) == ncol(miss)
  )

  bt_as_tibble(df[keep, , drop = FALSE])
}
