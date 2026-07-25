test_that("transmute keeps only new columns", {
  out <- transmute(mtcars, ratio = mpg / cyl)

  expect_s3_class(out, "data.table")
  expect_equal(names(out), "ratio")
})
