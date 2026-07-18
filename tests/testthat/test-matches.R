test_that("matches detects regex matches", {
  x <- c("abc", "xyz", "AbC")

  expect_equal(matches(x, "^a"), c(TRUE, FALSE, FALSE))
  expect_equal(matches(x, "^a", ignore_case = TRUE), c(TRUE, FALSE, TRUE))
})
