test_that("merge joins two tables", {
  x <- data.frame(id = c(1, 2), value_x = c("a", "b"))
  y <- data.frame(id = c(1, 3), value_y = c("c", "d"))

  out <- merge(x, y, by = "id", all.x = TRUE)

  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 2L)
  expect_true(all(c("id", "value_x", "value_y") %in% names(out)))
})
