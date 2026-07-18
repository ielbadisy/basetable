test_that("upper converts text to uppercase", {
  x <- c("AbC", "xyz")

  expect_equal(upper(x), c("ABC", "XYZ"))
})
