trim <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  trimws(x)
}
