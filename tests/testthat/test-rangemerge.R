test_that("rangemerge keeps only y rows whose value falls within x's [lower, upper]", {
  x <- data.frame(id = c(1, 1, 2), lower = c(0, 0, 0), upper = c(10, 10, 20))
  y <- data.frame(id = c(1, 1, 2), val = c(5, 15, 5), label = c("a", "b", "c"))

  out <- rangemerge(x, y, by = "id", lower = "lower", upper = "upper", value = "val")

  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 3L)
  expect_equal(out$label, c("a", "a", "c"))
  expect_false("val_end" %in% names(out))
})

test_that("rangemerge keeps unmatched x rows with NA y columns", {
  x <- data.frame(id = 1, lower = 100, upper = 200)
  y <- data.frame(id = 1, val = c(5, 15), label = c("a", "b"))

  out <- rangemerge(x, y, by = "id", lower = "lower", upper = "upper", value = "val")

  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$label))
})
