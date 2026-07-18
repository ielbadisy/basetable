test_that("glimpse returns the input invisibly", {
  out <- glimpse(iris)
  expect_identical(out, iris)
})
