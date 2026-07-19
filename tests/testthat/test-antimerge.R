test_that("antimerge keeps non-matching rows from x", {
  x <- data.frame(id = c(1, 2, 3), value = c("a", "b", "c"))
  y <- data.frame(id = c(2, 4), flag = c(TRUE, TRUE))

  out <- antimerge(x, y, by = "id")

  expect_s3_class(out, "data.table")
  expect_equal(out$id, c(1, 3))
  expect_equal(out$value, c("a", "c"))
})

test_that("antimerge errors clearly on an empty by=", {
  x <- data.frame(id = 1:2)
  y <- data.frame(id = 1:2)
  expect_error(antimerge(x, y, by = character(0)), "must contain at least one column")
})
