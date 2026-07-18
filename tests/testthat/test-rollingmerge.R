test_that("rollingmerge joins using a rolling key", {
  x <- data.frame(id = c(1, 1, 1), time = c(5, 10, 15))
  y <- data.frame(id = c(1, 1, 1), time = c(3, 8, 14), value = c("a", "b", "c"))

  out <- rollingmerge(x, y, by = c("id", "time"), direction = "backward")

  expect_s3_class(out, "data.table")
  expect_true("value" %in% names(out))
  expect_equal(out$value, c("a", "b", "c"))
})
