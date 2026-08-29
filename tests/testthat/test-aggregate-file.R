test_that("aggregate: single/multi key, multi fun", {
  set.seed(1); n <- 2e5L
  df <- data.frame(g = sample(c("a", "b", "c"), n, TRUE),
                   k = sample.int(40L, n, TRUE),
                   x = rnorm(n), y = runif(n), stringsAsFactors = FALSE)
  p <- tempfile(fileext = ".csv"); btwrite(df, p)

  r1 <- aggregate(p, by = "k", value = "x", fun = c("sum", "mean", "sd", "min", "max", "n"))
  groups <- base::split(df$x, df$k)
  d1 <- data.frame(
    k = as.integer(names(groups)),
    x_sum = vapply(groups, sum, numeric(1)),
    x_mean = vapply(groups, mean, numeric(1)),
    x_sd = vapply(groups, sd, numeric(1)),
    x_min = vapply(groups, min, numeric(1)),
    x_max = vapply(groups, max, numeric(1)),
    x_n = lengths(groups)
  )
  m <- merge(r1, d1, by = "k")
  for (f in c("sum", "mean", "sd", "min", "max")) {
    expect_equal(m[[paste0("x_", f, ".x")]], m[[paste0("x_", f, ".y")]], tolerance = 1e-7)
  }
  expect_identical(m$x_n.x, as.integer(m$x_n.y))

  r2 <- aggregate(p, by = c("g", "k"), value = c("x", "y"), fun = "mean")
  d2 <- aggregate(df, by = c("g", "k"), value = c("x", "y"), fun = mean, sort = FALSE)
  m2 <- merge(r2, d2, by = c("g", "k"))
  expect_equal(m2$x.x, m2$x.y, tolerance = 1e-7)
  expect_equal(m2$y.x, m2$y.y, tolerance = 1e-7)
})

test_that("aggregate: predicate pushdown (where) fused into the pass", {
  set.seed(2); n <- 2e5L
  df <- data.frame(g = sample(c("a", "b", "c"), n, TRUE),
                   k = sample.int(50L, n, TRUE), x = rnorm(n), y = runif(n),
                   stringsAsFactors = FALSE)
  p <- tempfile(fileext = ".csv"); btwrite(df, p)

  r <- aggregate(p, by = "g", value = "x", fun = "mean", where = c("x > 0", "y < 0.5"))
  d <- aggregate(df[df$x > 0 & df$y < 0.5, , drop = FALSE], by = "g", value = "x", fun = mean)
  expect_equal(merge(r, d, by = "g")$x.x, merge(r, d, by = "g")$x.y, tolerance = 1e-8)

  rs <- aggregate(p, by = "k", value = "x", fun = c("sum", "n"), where = "g == b")
  groups <- base::split(df$x[df$g == "b"], df$k[df$g == "b"])
  ds <- data.frame(k = as.integer(names(groups)), x_sum = vapply(groups, sum, numeric(1)), x_n = lengths(groups))
  ms <- merge(rs, ds, by = "k")
  expect_equal(ms$x_sum.x, ms$x_sum.y, tolerance = 1e-7)
  expect_identical(ms$x_n.x, as.integer(ms$x_n.y))
})

test_that("count counts grouped rows", {
  set.seed(3); n <- 1e5L
  df <- data.frame(g = sample(letters[1:5], n, TRUE), k = sample.int(20L, n, TRUE))
  p <- tempfile(fileext = ".csv"); btwrite(df, p)

  c1 <- count(p, by = "g")
  ref <- count(df, by = "g")
  expect_equal(bt_engine_order(c1, "g")$n, bt_engine_order(ref, "g")$n)

  c2 <- count(p, by = c("g", "k"), where = "k >= 10")
  d2 <- count(df[df$k >= 10, , drop = FALSE], by = c("g", "k"), sort = FALSE)
  expect_equal(nrow(c2), nrow(d2))
  expect_identical(merge(c2, d2, by = c("g", "k"))$n.x,
                   merge(c2, d2, by = c("g", "k"))$n.y)
})

test_that("distinct and freq", {
  set.seed(4); n <- 1e5L
  df <- data.frame(g = sample(letters[1:4], n, TRUE), k = sample.int(15L, n, TRUE))
  p <- tempfile(fileext = ".csv"); btwrite(df, p)

  d <- distinct(p, cols = c("g", "k"))
  u <- uniquerows(df, cols = c("g", "k"))
  expect_equal(nrow(d), nrow(u))
  expect_equal(nrow(merge(d, u, by = c("g", "k"))), nrow(u))

  f <- freq(p, by = "g")
  ref <- count(df, by = "g")
  expect_equal(bt_engine_order(f, "g")$n, bt_engine_order(ref, "g")$n)
  expect_equal(sum(f$prop), 1)
  expect_true(all(diff(f$n) <= 0))   # sorted desc
})
