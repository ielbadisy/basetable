textdist <- function(x, y = NULL, ignore_case = FALSE) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (!is.null(y) && is.factor(y)) {
    y <- as.character(y)
  }

  if (ignore_case) {
    x <- tolower(x)
    if (!is.null(y)) {
      y <- tolower(y)
    }
  }

  if (is.null(y)) {
    y <- x
  }

  utils::adist(x, y)
}
