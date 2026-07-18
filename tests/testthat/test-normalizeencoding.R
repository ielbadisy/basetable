test_that("normalizeencoding converts text to UTF-8", {
  x <- iconv("café", from = "UTF-8", to = "latin1")

  expect_equal(normalizeencoding(x), "café")
})
