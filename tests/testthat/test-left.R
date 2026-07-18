test_that("left takes leading characters", {
  x <- c("abcdef", "xyz")

  expect_equal(left(x, 3), c("abc", "xyz"))
})
