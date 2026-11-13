#' Repeat rows in a table
#'
#' Repeat each row of a table according to a count vector.
#' @param data A data frame or data table.
#' @param times A scalar or row-wise count vector giving repetitions.
#' @return A basetable with repeated rows.
#' @export
expandrows <- function(data, times) {
  df <- bt_as_data_frame(data)

  if (length(times) == 0L) {
    stop("`times` must not be empty.", call. = FALSE)
  }

  times <- bt_recycle_flag(times, nrow(df), "times")
  if (anyNA(times)) {
    stop("`times` must not contain missing values.", call. = FALSE)
  }
  if (any(times < 0)) {
    stop("`times` must be non-negative.", call. = FALSE)
  }
  if (any(abs(times - round(times)) > .Machine$double.eps^0.5)) {
    stop("`times` must contain whole numbers.", call. = FALSE)
  }

  idx <- rep.int(seq_len(nrow(df)), as.integer(times))
  bt_as_data_table(df[idx, , drop = FALSE])
}
