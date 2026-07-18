test_that("ncols returns the number of columns", {
  expect_equal(ncols(iris), 5L)
  expect_equal(ncols(data.frame(a = 1:3, b = 4:6)), 2L)
})
