splittext <- function(x, pattern) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (length(pattern) == 0L) {
    stop("`pattern` must not be empty.", call. = FALSE)
  }

  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    value <- x[[i]]
    if (is.na(value)) {
      out[[i]] <- NA_character_
    } else {
      out[[i]] <- strsplit(value, pattern[[1L]], fixed = TRUE)[[1L]]
    }
  }

  out
}
