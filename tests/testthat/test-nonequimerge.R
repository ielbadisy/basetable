test_that("nonequimerge returns a data.table for keyed merges", {
  x <- data.frame(id = c(1, 2), value = c("a", "b"))
  y <- data.frame(id = c(1, 2), label = c("c", "d"))

  out <- nonequimerge(x, y, by = "id")

  expect_s3_class(out, "data.table")
  expect_true("label" %in% names(out))
})
