test_that("normalizeunicode normalizes Unicode text", {
  testthat::skip_if_not_installed("stringi")

  x <- "e\u0301"

  expect_equal(normalizeunicode(x, form = "NFC"), "é")
  expect_equal(normalizeunicode(factor(x), form = "NFC"), "é")
})
