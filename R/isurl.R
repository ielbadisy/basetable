isurl <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  grepl("^(https?|ftp)://[^[:space:]]+$", x, perl = TRUE, ignore.case = TRUE)
}
