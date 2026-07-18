collapsetext <- function(x, sep = ", ") {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  paste(x, collapse = sep)
}
