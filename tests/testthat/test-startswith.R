test_that("startswith detects prefixes", {
  x <- c("abc", "xyz", "AbC")

  expect_equal(startswith(x, "a"), c(TRUE, FALSE, FALSE))
  expect_equal(startswith(x, "A", ignore_case = TRUE), c(TRUE, FALSE, TRUE))
})
