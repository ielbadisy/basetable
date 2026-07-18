normalizeencoding <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  enc2utf8(x)
}
