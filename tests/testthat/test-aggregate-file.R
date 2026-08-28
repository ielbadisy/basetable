test_that("bt_aggregate: single/multi key, multi fun, matches data.table", {
  skip_if_not_installed("data.table")
  set.seed(1); n <- 2e5L
  df <- data.frame(g = sample(c("a", "b", "c"), n, TRUE),
                   k = sample.int(40L, n, TRUE),
                   x = rnorm(n), y = runif(n), stringsAsFactors = FALSE)
  p <- tempfile(fileext = ".csv"); data.table::fwrite(df, p)
  dt <- data.table::as.data.table(df)

  r1 <- data.table::as.data.table(
    bt_aggregate(p, by = "k", value = "x", fun = c("sum", "mean", "sd", "min", "max", "n")))
  d1 <- dt[, .(x_sum = sum(x), x_mean = mean(x), x_sd = sd(x),
               x_min = min(x), x_max = max(x), x_n = .N), keyby = k]
  m <- merge(r1, d1, by = "k")
  for (f in c("sum", "mean", "sd", "min", "max"))
    expect_equal(m[[paste0("x_", f, ".x")]], m[[paste0("x_", f, ".y")]], tolerance = 1e-7)
  expect_identical(m$x_n.x, m$x_n.y)

  r2 <- data.table::as.data.table(bt_aggregate(p, by = c("g", "k"), value = c("x", "y"), fun = "mean"))
  d2 <- dt[, .(x = mean(x), y = mean(y)), keyby = .(g, k)]
  m2 <- merge(r2, d2, by = c("g", "k"))
  expect_equal(m2$x.x, m2$x.y, tolerance = 1e-7)
  expect_equal(m2$y.x, m2$y.y, tolerance = 1e-7)
})

test_that("bt_aggregate: predicate pushdown (where) fused into the pass", {
  skip_if_not_installed("data.table")
  set.seed(2); n <- 2e5L
  df <- data.frame(g = sample(c("a", "b", "c"), n, TRUE),
                   k = sample.int(50L, n, TRUE), x = rnorm(n), y = runif(n),
                   stringsAsFactors = FALSE)
  p <- tempfile(fileext = ".csv"); data.table::fwrite(df, p)
  dt <- data.table::as.data.table(df)

  r <- data.table::as.data.table(
    bt_aggregate(p, by = "g", value = "x", fun = "mean", where = c("x > 0", "y < 0.5")))
  d <- dt[x > 0 & y < 0.5, .(x = mean(x)), keyby = g]
  expect_equal(merge(r, d, by = "g")$x.x, merge(r, d, by = "g")$x.y, tolerance = 1e-8)

  rs <- data.table::as.data.table(
    bt_aggregate(p, by = "k", value = "x", fun = c("sum", "n"), where = "g == b"))
  ds <- dt[g == "b", .(x_sum = sum(x), x_n = .N), keyby = k]
  ms <- merge(rs, ds, by = "k")
  expect_equal(ms$x_sum.x, ms$x_sum.y, tolerance = 1e-7)
  expect_identical(ms$x_n.x, ms$x_n.y)
})

test_that("bt_count matches data.table .N", {
  skip_if_not_installed("data.table")
  set.seed(3); n <- 1e5L
  df <- data.frame(g = sample(letters[1:5], n, TRUE), k = sample.int(20L, n, TRUE))
  p <- tempfile(fileext = ".csv"); data.table::fwrite(df, p)
  dt <- data.table::as.data.table(df)

  c1 <- data.table::as.data.table(bt_count(p, by = "g"))
  expect_identical(c1[order(g)], dt[, .(n = .N), keyby = g], ignore_attr = TRUE)

  c2 <- data.table::as.data.table(bt_count(p, by = c("g", "k"), where = "k >= 10"))
  d2 <- dt[k >= 10, .(n = .N), keyby = .(g, k)]
  expect_equal(nrow(c2), nrow(d2))
  expect_identical(merge(c2, d2, by = c("g", "k"))$n.x,
                   merge(c2, d2, by = c("g", "k"))$n.y)
})
