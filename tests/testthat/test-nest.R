test_that("nest() gives one row per group in first-appearance order", {
  d <- data.frame(g = c("b", "a", "b"), x = 1:3, y = c(.1, .2, .3))
  n <- nest(d, by = "g")
  expect_s3_class(n, "basetable")
  expect_equal(n$g, c("b", "a"))
  expect_equal(names(n), c("g", "data"))
  expect_equal(nrow(n$data[[1]]), 2L)
  expect_equal(names(n$data[[1]]), c("x", "y"))
})

test_that("nest() honours name, multiple keys and NA keys", {
  d <- data.frame(g = c("a", NA, NA), h = 1, x = 1:3)
  n <- nest(d, by = c("g", "h"), name = "rows")
  expect_equal(nrow(n), 2L)
  expect_true("rows" %in% names(n))
  expect_equal(nrow(n$rows[[2]]), 2L)
})

test_that("nest() validates input", {
  d <- data.frame(g = 1, x = 2)
  expect_error(nest(d, by = "zz"), "Unknown columns")
  expect_error(nest(d, by = "g", name = "g"), "must not be one of")
  expect_error(nest(d, by = "g", name = ""), "non-empty")
})

test_that("unnest(nest(x)) round-trips up to row order", {
  d <- data.frame(g = c("a", "b", "a", "b"), x = 1:4, z = letters[1:4])
  back <- as.data.frame(unnest(nest(d, by = "g"), "data"))
  back <- back[order(back$x), c("g", "x", "z")]
  rownames(back) <- NULL
  expect_equal(back, d)
})

test_that("unnest() handles vector elements, empty elements and errors", {
  d <- data.frame(id = 1:3)
  d$v <- list(1:2, integer(0), 5L)
  u <- unnest(d, "v")
  expect_equal(u$id, c(1L, 1L, 3L))
  expect_equal(u$v, c(1L, 2L, 5L))
  expect_error(unnest(d, "id"), "Not list-columns")
  d$w <- list(1, 2, 3)
  expect_error(unnest(d, c("v", "w")), "matching element lengths")
})

test_that("nest() works on zero-row input", {
  n <- nest(data.frame(g = character(0), x = integer(0)), by = "g")
  expect_equal(nrow(n), 0L)
})
