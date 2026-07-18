isalphanumeric <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  grepl("^[[:alnum:]]+$", x, perl = TRUE)
}
