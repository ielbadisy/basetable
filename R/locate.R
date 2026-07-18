locate <- function(x, pattern, ignore_case = FALSE) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (ignore_case) {
    x <- tolower(x)
    pattern <- tolower(pattern)
  }

  pos <- regexpr(pattern, x, perl = TRUE)
  pos <- unname(as.integer(pos))
  pos[pos < 0L] <- NA_integer_
  pos
}
