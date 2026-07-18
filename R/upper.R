upper <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  toupper(x)
}
