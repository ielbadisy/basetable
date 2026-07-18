endswith <- function(x, suffix, ignore_case = FALSE) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (ignore_case) {
    x <- tolower(x)
    suffix <- tolower(suffix)
  }

  endsWith(x, suffix)
}
