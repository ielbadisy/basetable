# Regenerate the .bt_ascii_map / .bt_greek_map / .bt_cyrillic_map tables in
# R/transliterate.R from ICU, so removeaccents() / transliterate() match
# stringi::stri_trans_general() on the ranges they cover.
#
#   Rscript bench/make-translit-maps.R > /tmp/maps.R
#   # then paste the block into R/transliterate.R
#
# Requires stringi (a dev-only dependency; the package itself no longer uses it).

stopifnot(requireNamespace("stringi", quietly = TRUE))

emit <- function(cps, id, name) {
  ch  <- intToUtf8(cps, multiple = TRUE)
  icu <- stringi::stri_trans_general(ch, id)
  keep <- ch != icu & grepl("^[\x20-\x7e]+$", icu) & !icu %in% c("*", "/")
  ch <- ch[keep]; icu <- icu[keep]
  cat(sprintf("%s <- c(\n", name))
  for (i in seq_along(ch)) {
    cat(sprintf('  "\\u%04x" = %s%s\n',
                utf8ToInt(ch[i]), shQuote(icu[i], type = "cmd"),
                if (i == length(ch)) "" else ","))
  }
  cat(")\n\n")
}

emit(c(0xC0:0xFF, 0x100:0x17F, 0x218:0x21B, 0x1E9E), "Latin-ASCII", ".bt_ascii_map")
emit(0x370:0x3FF, "Any-Latin; Latin-ASCII", ".bt_greek_map")
emit(0x400:0x45F, "Any-Latin; Latin-ASCII", ".bt_cyrillic_map")
