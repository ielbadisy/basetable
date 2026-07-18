middle <- function(x, start, end) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (length(start) == 0L || length(end) == 0L) {
    stop("`start` and `end` must not be empty.", call. = FALSE)
  }
  start <- bt_recycle_flag(start, length(x), "start")
  end <- bt_recycle_flag(end, length(x), "end")
  if (any(start < 1L)) {
    stop("`start` must be at least 1.", call. = FALSE)
  }
  if (any(end < start)) {
    stop("`end` must be greater than or equal to `start`.", call. = FALSE)
  }

  substr(x, start, end)
}
