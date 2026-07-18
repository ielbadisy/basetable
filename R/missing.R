#' Summarize missing values by column or row
#'
#' @param data A data.frame or data.table.
#' @param margin Summarize missingness `"column"`-wise (the default) or
#'   `"row"`-wise.
#'
#' @return A tibble with one row per column (or per row) describing the count
#'   and proportion of missing values.
#' @export
missingness <- function(data, margin = c("column", "row")) {
  margin <- match.arg(margin)
  df <- bt_as_data_frame(data)

  if (margin == "column") {
    return(bt_as_tibble(data.frame(
      column = names(df),
      missing = vapply(df, function(x) sum(is.na(x)), numeric(1)),
      missing_prop = vapply(df, function(x) mean(is.na(x)), numeric(1)),
      complete = vapply(df, function(x) sum(!is.na(x)), numeric(1)),
      stringsAsFactors = FALSE
    )))
  }

  bt_as_tibble(data.frame(
    row = seq_len(nrow(df)),
    missing = rowSums(is.na(df)),
    complete = stats::complete.cases(df),
    stringsAsFactors = FALSE
  ))
}
