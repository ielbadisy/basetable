removetext <- function(x, pattern) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  sub(pattern, "", x, fixed = TRUE)
}
