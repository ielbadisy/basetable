countmatch <- function(x, pattern, ignore_case = FALSE) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (ignore_case) {
    x <- tolower(x)
    pattern <- tolower(pattern)
  }

  matches <- gregexpr(pattern, x, perl = TRUE)
  out <- lengths(regmatches(x, matches))
  out[is.na(x)] <- NA_integer_
  out
}
