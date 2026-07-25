test_that("distinct returns unique rows", {
  out <- distinct(mtcars, cols = "cyl")

  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), length(unique(mtcars$cyl)))
})
