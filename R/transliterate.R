transliterate <- function(x, rule = "Any-Latin; Latin-ASCII") {
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("`stringi` is required for `transliterate()`.", call. = FALSE)
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  stringi::stri_trans_general(x, rule)
}
