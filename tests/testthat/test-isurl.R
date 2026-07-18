test_that("isurl detects URL-like strings", {
  x <- c("https://example.com", "http://example.com/path", "ftp://example.com", "bad", NA_character_)

  expect_equal(isurl(x), c(TRUE, TRUE, TRUE, FALSE, FALSE))
})
