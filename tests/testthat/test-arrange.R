test_that("arrange sorts rows", {
  out <- arrange(mtcars, by = "mpg")

  expect_s3_class(out, "data.table")
  expect_false(is.unsorted(out$mpg, strictly = FALSE))
})
