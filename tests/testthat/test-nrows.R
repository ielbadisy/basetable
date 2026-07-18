test_that("nrows returns the number of rows", {
  expect_equal(nrows(iris), 150L)
  expect_equal(nrows(data.frame(a = 1:3)), 3L)
})
