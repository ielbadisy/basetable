test_that("pick returns only selected columns", {
  out <- pick(mtcars, c("mpg", "cyl"))

  expect_s3_class(out, "basetable")
  expect_equal(names(out), c("mpg", "cyl"))
})
