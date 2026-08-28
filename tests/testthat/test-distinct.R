test_that("uniquerows returns unique rows", {
  out <- uniquerows(mtcars, cols = "cyl")

  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), length(unique(mtcars$cyl)))
})

test_that("uniquerows with no cols returns unique rows of the whole table", {
  df <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))

  out <- uniquerows(df)

  expect_equal(nrow(out), 2L)
})

test_that("uniquerows does not mutate its input", {
  input <- data.frame(id = c(1L, 1L, 2L), value = c(3, 3, 1))
  original <- input

  uniquerows(input)

  expect_identical(input, original)
})
