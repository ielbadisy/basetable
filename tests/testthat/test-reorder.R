test_that("reorder sorts rows", {
  out <- reorder(mtcars, by = "mpg")

  expect_s3_class(out, "data.table")
  expect_false(is.unsorted(out$mpg, strictly = FALSE))
})
