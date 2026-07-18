titlecase <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  tools::toTitleCase(x)
}
