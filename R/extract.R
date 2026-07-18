extract <- function(x, pattern, group = 1L, ignore_case = FALSE) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (length(group) == 0L) {
    stop("`group` must not be empty.", call. = FALSE)
  }
  group <- bt_recycle_flag(group, length(x), "group")
  if (any(group < 0L)) {
    stop("`group` must be non-negative.", call. = FALSE)
  }

  matches <- regexec(pattern, x, perl = TRUE, ignore.case = ignore_case)
  values <- regmatches(x, matches)
  out <- rep(NA_character_, length(x))

  for (i in seq_along(values)) {
    hit <- values[[i]]
    if (length(hit) == 0L) {
      next
    }

    idx <- group[[i]] + 1L
    if (idx <= length(hit)) {
      out[[i]] <- hit[[idx]]
    }
  }

  out
}
