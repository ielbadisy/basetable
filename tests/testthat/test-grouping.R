# ---- aggregate -------------------------------------------------------------

test_that("aggregate summarizes grouped values", {
  out <- aggregate(mtcars, by = "cyl", value = "mpg", fun = mean)

  expect_s3_class(out, "basetable")
  expect_true(all(c("cyl", "mpg") %in% names(out)))
  expect_equal(nrow(out), length(unique(mtcars$cyl)))
})

# ---- count -----------------------------------------------------------------

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

# ---- distinct --------------------------------------------------------------

test_that("uniquerows returns unique rows", {
  out <- uniquerows(mtcars, cols = "cyl")

  expect_s3_class(out, "basetable")
  expect_equal(nrow(out), length(unique(mtcars$cyl)))
})

test_that("uniquerows with no cols returns unique rows of the whole table", {
  df <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))

  out <- uniquerows(df)

  expect_equal(nrow(out), 2L)
})

test_that("uniquerows does not mutate its input", {
  input <- data.frame(id = c(1L, 1L, 2L), value = c(3, 3, 1))
  original <- input

  uniquerows(input)

  expect_identical(input, original)
})

# ---- summaries -------------------------------------------------------------

test_that("summaries returns named summaries", {
  out <- summaries(mtcars, by = "cyl", mean_mpg = mean(mpg), n = length(mpg))

  expect_s3_class(out, "basetable")
  expect_true(all(c("cyl", "mean_mpg", "n") %in% names(out)))
  expect_equal(nrow(out), length(unique(mtcars$cyl)))
})

# ---- freq ------------------------------------------------------------------

test_that("freq counts values", {
  out <- freq(iris, column = "Species")

  expect_s3_class(out, "basetable")
  expect_true(all(c("Species", "n") %in% names(out)))
  expect_equal(sum(out$n), nrow(iris))
})

# ---- split -----------------------------------------------------------------

test_that("split returns a list of table pieces", {
  out <- split(iris, by = "Species")

  expect_type(out, "list")
  expect_equal(length(out), 3L)
  expect_s3_class(out[[1]], "basetable")
})

test_that("split falls back to base::split() for non-data-frame input", {
  x <- 1:10
  f <- rep(1:2, 5)

  expect_equal(split(x, f), base::split(x, f))
  expect_equal(split(letters[1:6], rep(c("a", "b"), 3)),
               base::split(letters[1:6], rep(c("a", "b"), 3)))
})
