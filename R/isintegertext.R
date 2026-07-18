isintegertext <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  grepl("^[+-]?[0-9]+$", x, perl = TRUE)
}
