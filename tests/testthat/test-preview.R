test_that("preview returns the input invisibly", {
  out <- preview(iris)
  expect_identical(out, iris)
})
