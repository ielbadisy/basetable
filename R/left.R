left <- function(x, n = 1L) {
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

  substr(x, 1L, n)
}
