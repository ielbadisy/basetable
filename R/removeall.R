removeall <- function(x, pattern) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  gsub(pattern, "", x, fixed = TRUE)
}
