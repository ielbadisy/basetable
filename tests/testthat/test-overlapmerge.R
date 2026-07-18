test_that("overlapmerge matches overlapping intervals", {
  x <- data.frame(id = c(1, 2), startx = c(1, 10), endx = c(4, 14))
  y <- data.frame(id = c(1, 2), starty = c(3, 12), endy = c(5, 15), value = c("a", "b"))

  out <- overlapmerge(x, y, startx = "startx", endx = "endx", starty = "starty", endy = "endy", by = "id")

  expect_s3_class(out, "tbl_df")
  expect_true("value" %in% names(out))
  expect_equal(out$value, c("a", "b"))
})
