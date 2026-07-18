right <- function(x, n = 1L) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (length(n) == 0L) {
    stop("`n` must not be empty.", call. = FALSE)
  }
  n <- bt_recycle_flag(n, length(x), "n")
  if (any(n < 0)) {
    stop("`n` must be non-negative.", call. = FALSE)
  }

  len <- nchar(x)
  start <- pmax(len - n + 1L, 1L)
  substr(x, start, len)
}
