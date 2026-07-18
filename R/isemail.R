isemail <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  grepl("^[^@[:space:]]+@[^@[:space:]]+[.][^@[:space:]]+$", x, perl = TRUE)
}
