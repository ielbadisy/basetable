# ---- stack -----------------------------------------------------------------

test_that("stack returns a long table", {
  out <- stack(data.frame(a = 1:2, b = 3:4))

  expect_s3_class(out, "basetable")
  expect_true(all(c("values", "ind") %in% names(out)))
})

# ---- unstack ---------------------------------------------------------------

test_that("unstack reshapes a stacked table", {
  stacked <- data.frame(
    id = c(1, 2),
    var = c("a", "a"),
    value = c(10, 20)
  )

  out <- unstack(stacked, value ~ var)

  expect_s3_class(out, "basetable")
  expect_true("a" %in% names(out))
})

# ---- completegrid ----------------------------------------------------------

test_that("completegrid creates missing combinations", {
  data <- data.frame(site = c("A", "A", "B"), sex = c("M", "F", "M"), value = c(1, 2, 3))

  out <- completegrid(data, cols = c("site", "sex"))

  expect_s3_class(out, "basetable")
  expect_equal(nrow(out), 4L)
  expect_true(any(is.na(out$value)))
})

# ---- expandrows ------------------------------------------------------------

test_that("expandrows repeats rows by count", {
  data <- data.frame(id = c(1, 2), value = c("a", "b"))

  out <- expandrows(data, times = c(2, 1))

  expect_s3_class(out, "basetable")
  expect_equal(out$id, c(1, 1, 2))
  expect_equal(out$value, c("a", "a", "b"))
})

# ---- nest and unnest --------------------------------------------------------

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

test_that("nest() groups integer, logical and wide-range keys", {
  d <- data.frame(g = c(5L, NA, -3L, 5L, NA), x = 1:5)
  n <- nest(d, by = "g")
  expect_equal(n$g, c(5L, NA, -3L))
  expect_equal(n$data[[1]]$x, c(1L, 4L))
  expect_equal(n$data[[2]]$x, c(2L, 5L))

  wide <- data.frame(g = c(1L, 2000000000L, 1L, -2000000000L), x = 1:4)
  nw <- nest(wide, by = "g")
  expect_equal(nw$g, c(1L, 2000000000L, -2000000000L))
  expect_equal(nw$data[[1]]$x, c(1L, 3L))

  lg <- nest(data.frame(g = c(TRUE, NA, TRUE), x = 1:3), by = "g")
  expect_equal(lg$g, c(TRUE, NA))
})

test_that("nest() and unnest() keep factor, Date and list columns", {
  d <- data.frame(
    g = c("a", "b", "a"),
    f = factor(c("u", "v", "u"), levels = c("v", "u", "w")),
    dt = as.Date("2020-01-01") + 0:2
  )
  d$l <- list(1, "a", 1:2)
  n <- nest(d, by = "g")
  expect_equal(levels(n$data[[1]]$f), c("v", "u", "w"))
  expect_s3_class(n$data[[1]]$dt, "Date")
  expect_equal(n$data[[1]]$l, list(1, 1:2))
  back <- as.data.frame(unnest(n, "data"))
  expect_equal(back$f, factor(c("u", "u", "v"), levels = c("v", "u", "w")))
  expect_equal(back$dt, as.Date("2020-01-01") + c(0L, 2L, 1L))
})

test_that("unnest() falls back when nested frames differ", {
  d <- data.frame(id = 1:2)
  d$data <- list(data.frame(a = 1:2), data.frame(a = 3L, b = "x"))
  u <- unnest(d, "data")
  expect_equal(u$id, c(1L, 1L, 2L))
  expect_equal(u$a, 1:3)
  expect_equal(u$b, c(NA, NA, "x"))

  f <- data.frame(id = 1:2)
  f$data <- list(
    data.frame(k = factor("a")), data.frame(k = factor("b"))
  )
  expect_equal(nrow(unnest(f, "data")), 2L)
})

test_that("nest() and unnest() agree with tidyr", {
  skip_if_not_installed("tidyr")
  set.seed(1)
  d <- data.frame(
    g = sample(c(1:4, NA), 200, TRUE),
    x = rnorm(200),
    s = sample(c(letters, NA), 200, TRUE)
  )
  ours <- unnest(nest(d, by = "g"), "data")
  ref <- tidyr::unnest(tidyr::nest(d, data = c(x, s), .by = "g"), data)
  key <- function(u) as.data.frame(u)[c("g", "x", "s")]
  o <- key(ours); r <- key(ref)
  o <- o[do.call(order, o), ]; r <- r[do.call(order, r), ]
  rownames(o) <- rownames(r) <- NULL
  expect_equal(o, r)
})
