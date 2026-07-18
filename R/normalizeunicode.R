normalizeunicode <- function(x, form = c("NFC", "NFD", "NFKC", "NFKD")) {
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("`stringi` is required for `normalizeunicode()`.", call. = FALSE)
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  form <- match.arg(form)
  fun <- switch(
    form,
    NFC = stringi::stri_trans_nfc,
    NFD = stringi::stri_trans_nfd,
    NFKC = stringi::stri_trans_nfkc,
    NFKD = stringi::stri_trans_nfkd
  )

  fun(x)
}
