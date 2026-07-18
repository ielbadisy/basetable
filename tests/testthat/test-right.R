test_that("right takes trailing characters", {
  x <- c("abcdef", "xyz")

  expect_equal(right(x, 3), c("def", "xyz"))
})
