matches <- function(x, pattern, ignore_case = FALSE) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  grepl(pattern, x, ignore.case = ignore_case)
}
