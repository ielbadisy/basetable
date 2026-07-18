contains <- function(x, pattern, ignore_case = FALSE) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (ignore_case) {
    x <- tolower(x)
    pattern <- tolower(pattern)
  }

  grepl(pattern, x, fixed = TRUE)
}
