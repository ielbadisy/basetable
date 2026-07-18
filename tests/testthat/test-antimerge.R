test_that("antimerge keeps non-matching rows from x", {
  x <- data.frame(id = c(1, 2, 3), value = c("a", "b", "c"))
  y <- data.frame(id = c(2, 4), flag = c(TRUE, TRUE))

  out <- antimerge(x, y, by = "id")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$id, c(1, 3))
  expect_equal(out$value, c("a", "c"))
})
