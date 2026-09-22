test_that("string dictionaries retain keys across growth and missing values", {
  keys <- c(sprintf("key-%05d", 1:12000), NA_character_, "")
  g <- rep(keys, 2L)
  x <- data.frame(g = g, value = seq_along(g))
  expect_identical(uniquerows(x, cols = "g")$g, keys)
  counts <- count(x, by = "g", sort = FALSE)
  expect_identical(counts$g, keys)
  expect_identical(counts$n, rep(2L, length(keys)))
  y <- data.frame(g = keys, extra = seq_along(keys))
  old <- options("basetable.threads")
  on.exit(options(old), add = TRUE)
  for (threads in c(1L, 4L)) {
    options(basetable.threads = threads)
    out <- merge(x, y, by = "g")
    expect_identical(out$value, x$value)
    expect_identical(out$extra, rep(seq_along(keys), 2L))
    expect_identical(semimerge(x, y, by = "g")$value, x$value)
  }
})

test_that("all-row selection preserves columns and partial masks exclude NA", {
  x <- data.frame(g = c("a", NA, "b"), value = 1:3)
  all <- basetable:::bt_engine_subset(x, rows = rep(TRUE, 3))
  expect_identical(all$g, x$g)
  all$value[1] <- 99L
  expect_identical(x$value, 1:3)
  part <- basetable:::bt_engine_subset(x, rows = c(TRUE, NA, FALSE))
  expect_identical(part$value, 1L)
  empty <- basetable:::bt_engine_subset(x[FALSE, ], rows = logical())
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), names(x))
})

test_that("parallel string join compacts unmatched rows in input order", {
  x <- data.frame(g = rep(c("a", "missing", NA, "b"), 60000L),
                  value = seq_len(240000L))
  y <- data.frame(g = c("b", NA, "a"), extra = 1:3)
  matched <- match(x$g, y$g)
  old <- options(basetable.threads = 4L)
  on.exit(options(old), add = TRUE)
  inner <- merge(x, y, by = "g")
  expect_identical(inner$value, x$value[!is.na(matched)])
  expect_identical(inner$extra, y$extra[matched[!is.na(matched)]])
  left <- merge(x, y, by = "g", all.x = TRUE)
  expect_identical(left$value, x$value)
  expect_identical(left$extra, y$extra[matched])
  duplicate <- merge(x, y[c(1L, 1L), ], by = "g")
  expect_identical(duplicate$value, rep(x$value[x$g %in% "b"], each = 2L))
})

test_that("string numeric sorting preserves ties and handles skewed groups", {
  old <- options("basetable.threads")
  on.exit(options(old), add = TRUE)
  for (n in c(1000L, 70000L)) {
    x <- data.frame(g = rep("same", n),
                    x = rep(c(NA_real_, NaN, -Inf, -0, 0, 2, Inf), length.out = n),
                    id = seq_len(n))
    ref <- order(x$g, x$x, method = "radix", na.last = TRUE)
    for (threads in c(1L, 4L)) {
      options(basetable.threads = threads)
      expect_identical(orderrows(x, by = c("g", "x"))$id, x$id[ref])
    }
  }
})

test_that("group reducers read numeric storage consistently across types", {
  old <- options("basetable.threads")
  on.exit(options(old), add = TRUE)
  for (value in list(c(1, NA, NaN, 4), c(1L, NA_integer_, 3L, 4L),
                     c(TRUE, NA, FALSE, TRUE))) {
    x <- data.frame(g = rep(c("a", "b"), 200000L),
                    value = rep(value, 100000L))
    for (threads in c(1L, 4L)) {
      options(basetable.threads = threads)
      for (remove in c(FALSE, TRUE)) {
        out <- aggregate(x, by = "g", value = "value", fun = sd,
                         na.rm = remove, sort = FALSE)
        expected <- vapply(c("a", "b"), function(g)
          stats::sd(x$value[x$g == g], na.rm = remove), numeric(1))
        expect_equal(out$value, unname(expected))
      }
    }
  }
})
