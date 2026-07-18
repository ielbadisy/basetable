test_that("nearestmerge wraps rollingmerge nearest mode", {
  x <- data.frame(id = c(1, 3), value = c("a", "b"))
  y <- data.frame(id = c(2, 4), label = c("c", "d"))

  out <- nearestmerge(x, y, by = "id")

  expect_s3_class(out, "data.table")
  expect_true("id" %in% names(out))
})
