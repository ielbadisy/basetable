isalpha <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  grepl("^[[:alpha:]]+$", x, perl = TRUE)
}
