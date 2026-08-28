test_that("renamecols changes column names", {
  out <- renamecols(mtcars, miles = mpg, cylinders = cyl)

  expect_s3_class(out, "data.table")
  expect_true(all(c("miles", "cylinders") %in% names(out)))
})

test_that("renamecols does not mutate its input", {
  input <- data.frame(id = c(1L, 1L, 2L), value = c(3, 3, 1))
  original <- input

  renamecols(input, key = id)

  expect_identical(input, original)
})
