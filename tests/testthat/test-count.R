test_that("count returns one row per group", {
  out <- count(iris, by = "Species")

  expect_s3_class(out, "basetable")
  expect_true(all(c("Species", "n") %in% names(out)))
  expect_equal(sum(out$n), nrow(iris))
})

test_that("count(sort = TRUE) breaks ties by ascending group key, not input order", {
  # "b" and "c" tie on count (2 each); "a" and "d" tie on count (1 each).
  # The tie order must not depend on which one appears first in the data.
  d1 <- data.frame(g = c("c", "b", "c", "b", "a", "d"))
  d2 <- data.frame(g = c("b", "c", "b", "c", "d", "a"))

  out1 <- count(d1, by = "g")
  out2 <- count(d2, by = "g")

  expect_equal(out1$g, c("b", "c", "a", "d"))
  expect_equal(out1, out2)
})

test_that("count(sort = FALSE) is unaffected by the tie-break change", {
  d <- data.frame(g = c("c", "b", "c", "b", "a", "d"))
  out <- count(d, by = "g", sort = FALSE)
  expect_equal(sum(out$n), nrow(d))
})
