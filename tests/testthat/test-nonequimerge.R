test_that("nonequimerge returns a tibble for keyed merges", {
  x <- data.frame(id = c(1, 2), value = c("a", "b"))
  y <- data.frame(id = c(1, 2), label = c("c", "d"))

  out <- nonequimerge(x, y, by = "id")

  expect_s3_class(out, "tbl_df")
  expect_true("label" %in% names(out))
})
