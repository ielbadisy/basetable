test_that("drop removes selected columns", {
  out <- drop(mtcars, c("mpg", "cyl"))

  expect_s3_class(out, "basetable")
  expect_false(any(c("mpg", "cyl") %in% names(out)))
})
