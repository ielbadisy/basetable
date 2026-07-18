test_that("crossmerge returns the Cartesian product", {
  x <- data.frame(id = c(1, 2))
  y <- data.frame(label = c("a", "b", "c"))

  out <- crossmerge(x, y)

  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 6L)
})
