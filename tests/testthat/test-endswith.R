test_that("endswith detects suffixes", {
  x <- c("abc", "xyz", "AbC")

  expect_equal(endswith(x, "c"), c(TRUE, FALSE, FALSE))
  expect_equal(endswith(x, "C", ignore_case = TRUE), c(TRUE, FALSE, TRUE))
})
