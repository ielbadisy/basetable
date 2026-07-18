test_that("updatemerge updates selected columns", {
  x <- data.frame(id = c(1, 2, 3), value = c("a", "b", "c"), other = c(1, 2, 3))
  y <- data.frame(id = c(2, 3), value = c("z", "y"))

  out <- updatemerge(x, y, by = "id", cols = "value")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$value, c("a", "z", "y"))
  expect_equal(out$other, c(1, 2, 3))
})
