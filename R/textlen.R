textlen <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  nchar(x)
}
