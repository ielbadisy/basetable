test_that("aggregate summarizes grouped values", {
  out <- aggregate(mtcars, by = "cyl", value = "mpg", fun = mean)

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("cyl", "mpg") %in% names(out)))
  expect_equal(nrow(out), length(unique(mtcars$cyl)))
})
