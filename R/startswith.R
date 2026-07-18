startswith <- function(x, prefix, ignore_case = FALSE) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (ignore_case) {
    x <- tolower(x)
    prefix <- tolower(prefix)
  }

  startsWith(x, prefix)
}
