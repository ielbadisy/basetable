removeaccents <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  iconv(x, from = "", to = "ASCII//TRANSLIT")
}
