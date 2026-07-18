test_that("transliterate converts text using stringi rules", {
  testthat::skip_if_not_installed("stringi")

  x <- c("café", "niño")

  expect_equal(transliterate(x), c("cafe", "nino"))
})
