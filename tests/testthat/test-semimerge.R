test_that("semimerge keeps matching rows from x", {
  x <- data.frame(id = c(1, 2, 3), value = c("a", "b", "c"))
  y <- data.frame(id = c(2, 3, 4), flag = c(TRUE, FALSE, TRUE))

  out <- semimerge(x, y, by = "id")

  expect_s3_class(out, "data.table")
  expect_equal(out$id, c(2, 3))
  expect_equal(out$value, c("b", "c"))
})

test_that("semimerge errors clearly on an empty by=", {
  x <- data.frame(id = 1:2)
  y <- data.frame(id = 1:2)
  expect_error(semimerge(x, y, by = character(0)), "must contain at least one column")
})
