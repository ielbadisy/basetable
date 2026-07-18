lower <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  tolower(x)
}
