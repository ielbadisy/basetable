test_that("contains detects literal substrings", {
  x <- c("abc", "xyz", "AbC")

  expect_equal(contains(x, "bc"), c(TRUE, FALSE, FALSE))
  expect_equal(contains(x, "ABC", ignore_case = TRUE), c(TRUE, FALSE, TRUE))
})
