test_that("compact drops only NULL elements", {
  out <- compact(list(1, NULL, 2, NULL, 3))
  expect_equal(out, list(1, 2, 3))
})

test_that("compact keeps falsy-but-not-NULL values", {
  out <- compact(list(a = 1, b = NULL, c = NA, d = 0, e = ""))
  expect_equal(out, list(a = 1, c = NA, d = 0, e = ""))
})

test_that("compact preserves names and order", {
  out <- compact(list(x = 1, y = NULL, z = 2))
  expect_equal(names(out), c("x", "z"))
})

test_that("compact returns an empty list unchanged", {
  expect_equal(compact(list()), list())
})

test_that("compact errors on non-list input", {
  expect_error(compact(c(1, 2, 3)), "must be a list")
})
