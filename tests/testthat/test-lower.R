test_that("lower converts text to lowercase", {
  x <- c("AbC", "XYZ")

  expect_equal(lower(x), c("abc", "xyz"))
})
