# ---- native engine paths ---------------------------------------------------

test_that("native engine powers projection and row filtering", {
  df <- data.frame(a = 1:5, b = letters[1:5], c = c(TRUE, FALSE, TRUE, NA, TRUE))

  out <- pick(df, c("b", "a"))
  expect_s3_class(out, "basetable")
  expect_equal(as.data.frame(out), df[c("b", "a")])

  filtered <- subset(df, c, select = c(a, b))
  expect_equal(as.data.frame(filtered), df[c(1L, 3L, 5L), c("a", "b")], ignore_attr = TRUE)
})

test_that("native engine handles integer count, unique, and duplicates", {
  df <- data.frame(g = c(2L, 1L, 2L, NA, 1L, NA), x = seq_len(6))

  cnt <- count(df, by = "g", sort = FALSE)
  expect_equal(cnt$g, c(2L, 1L, NA))
  expect_equal(cnt$n, c(2L, 2L, 2L))

  uniq <- uniquerows(df, cols = "g")
  expect_equal(uniq$g, c(2L, 1L, NA))

  dup <- duplicaterows(df[c("g")])
  expect_equal(dup$g, c(2L, 1L, 2L, NA, 1L, NA))

  first_only <- removeduplicates(df, by = "g", keep = "first")
  expect_equal(first_only$g, c(2L, 1L, NA))
  expect_equal(first_only$x, c(1L, 2L, 4L))
})

test_that("native engine orders without mutating inputs", {
  input <- data.frame(g = c(2L, 1L, 2L), x = c(3, 2, 1))
  original <- input

  out <- orderrows(input, by = c("g", "x"), decreasing = c(FALSE, TRUE))

  expect_equal(out$g, c(1L, 2L, 2L))
  expect_equal(out$x, c(2, 3, 1))
  expect_identical(input, original)
})

test_that("native engine aggregates common reducers", {
  df <- data.frame(g = c(1L, 1L, 2L, 2L), x = c(1, 3, 5, NA))

  expect_equal(aggregate(df, by = "g", value = "x", fun = sum, na.rm = TRUE)$x, c(4, 5))
  expect_equal(aggregate(df, by = "g", value = "x", fun = mean, na.rm = TRUE)$x, c(2, 5))
  expect_equal(aggregate(df, by = "g", value = "x", fun = min, na.rm = TRUE)$x, c(1, 5))
  expect_equal(aggregate(df, by = "g", value = "x", fun = max, na.rm = TRUE)$x, c(3, 5))
  expect_equal(aggregate(df, by = "g", value = "x", fun = var, na.rm = TRUE)$x, c(2, NA))
  expect_equal(aggregate(df, by = "g", value = "x", fun = sd, na.rm = TRUE)$x, c(sqrt(2), NA))
  expect_equal(aggregate(df, by = "g", value = "x", fun = "n")$x, c(2, 2))
  expect_true(is.na(aggregate(df, by = "g", value = "x", fun = sum, na.rm = FALSE)$x[[2L]]))
})

test_that("native engine powers semi and anti joins", {
  x <- data.frame(id = c(1L, 1L, 2L, 3L), grp = c("a", "b", "a", "a"), val = 1:4)
  y <- data.frame(id = c(1L, 3L), grp = c("b", "a"))

  semi <- semimerge(x, y, by = c("id", "grp"))
  anti <- antimerge(x, y, by = c("id", "grp"))

  expect_equal(semi$val, c(2L, 4L))
  expect_equal(anti$val, c(1L, 3L))
  expect_s3_class(semi, "basetable")
  expect_s3_class(anti, "basetable")
})

test_that("native engine materialises equi joins with all.x / all.y / suffixes", {
  x <- data.frame(id = c(1L, 2L, 2L, 4L), v = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  y <- data.frame(id = c(2L, 3L), v = c("Y2", "Y3"), w = c(10L, 20L), stringsAsFactors = FALSE)

  inner <- merge(x, y, by = "id")
  expect_equal(names(inner), c("id", "v.x", "v.y", "w"))
  expect_equal(inner$id, c(2L, 2L))
  expect_equal(inner$v.x, c("b", "c"))
  expect_equal(inner$v.y, c("Y2", "Y2"))

  left <- merge(x, y, by = "id", all.x = TRUE)
  expect_equal(left$id, c(1L, 2L, 2L, 4L))
  expect_equal(left$w, c(NA, 10L, 10L, NA))

  dense_x <- data.frame(id = c(1L, 3L, 9L, NA_integer_), v = 1:4)
  dense_y <- data.frame(id = 1:5, w = 11:15)
  dense <- merge(dense_x, dense_y, by = "id", all.x = TRUE)
  expect_equal(dense$id, dense_x$id)
  expect_equal(dense$w, c(11L, 13L, NA, NA))

  dup_y <- data.frame(id = c(1L, 3L, 3L), w = 21:23)
  dup <- merge(dense_x[1:2, ], dup_y, by = "id", all.x = TRUE)
  expect_equal(dup$id, c(1L, 3L, 3L))
  expect_equal(dup$w, c(21L, 22L, 23L))

  full <- merge(x, y, by = "id", all = TRUE, sort = TRUE)
  expect_equal(full$id, c(1L, 2L, 2L, 3L, 4L))
  expect_equal(full$v.y, c(NA, "Y2", "Y2", "Y3", NA))

  custom <- merge(x, y, by = "id", suffixes = c("_l", "_r"))
  expect_true(all(c("v_l", "v_r") %in% names(custom)))
})

test_that("native engine joins on multi-column keys and Cartesian products", {
  x <- data.frame(a = c(1L, 1L, 2L), b = c("p", "q", "p"), xv = 1:3, stringsAsFactors = FALSE)
  y <- data.frame(a = c(1L, 2L), b = c("p", "p"), yv = c(9L, 8L), stringsAsFactors = FALSE)

  j <- merge(x, y, by = c("a", "b"))
  expect_equal(j$xv, c(1L, 3L))
  expect_equal(j$yv, c(9L, 8L))

  cj <- crossmerge(data.frame(a = 1:2), data.frame(z = c("x", "y", "z")))
  expect_equal(nrow(cj), 6L)
})

test_that("native engine coerces mismatched numeric and factor join keys", {
  a <- data.frame(id = 1:4, x = letters[1:4], stringsAsFactors = FALSE)
  b <- data.frame(id = c(2, 3, 5), y = c("B", "C", "E"), stringsAsFactors = FALSE)

  inner <- merge(a, b, by = "id")
  expect_equal(inner$id, c(2, 3))
  expect_equal(inner$y, c("B", "C"))

  full <- merge(a, b, by = "id", all = TRUE, sort = TRUE)
  expect_equal(full$id, c(1, 2, 3, 4, 5))
  expect_equal(full$x, c("a", "b", "c", "d", NA))

  f1 <- data.frame(g = factor(c("lo", "hi", "lo")), v = 1:3)
  f2 <- data.frame(g = c("hi", "lo"), w = c(10, 20), stringsAsFactors = FALSE)
  fj <- merge(f1, f2, by = "g")
  expect_equal(fj$v, c(1L, 2L, 3L))
  expect_equal(fj$w, c(20, 10, 20))
})

test_that("native engine drives range, overlap and rolling joins", {
  x <- data.frame(id = c(1L, 1L), lower = c(0, 100), upper = c(10, 200))
  y <- data.frame(id = 1L, val = c(5, 15), label = c("a", "b"), stringsAsFactors = FALSE)
  rng <- rangemerge(x, y, by = "id", lower = "lower", upper = "upper", value = "val")
  expect_equal(rng$label, c("a", NA))

  xt <- data.frame(id = c(1L, 1L, 1L), time = c(5, 10, 15))
  yt <- data.frame(id = c(1L, 1L), time = c(3, 12), value = c("a", "b"), stringsAsFactors = FALSE)
  roll <- rollingmerge(xt, yt, by = c("id", "time"), direction = "backward")
  expect_equal(roll$value, c("a", "a", "b"))

  nearest <- rollingmerge(xt, yt, by = c("id", "time"), direction = "nearest")
  expect_equal(nearest$value, c("a", "b", "b"))
})

test_that("native row-bind unions columns, fills gaps and promotes types", {
  a <- data.frame(x = 1:2, y = c("a", "b"), stringsAsFactors = FALSE)
  b <- data.frame(y = c("c", "d"), z = c(TRUE, FALSE), stringsAsFactors = FALSE)

  bound <- rbindfill(list(a, b))
  expect_s3_class(bound, "basetable")
  expect_equal(names(bound), c("x", "y", "z"))
  expect_equal(bound$x, c(1L, 2L, NA, NA))
  expect_equal(bound$y, c("a", "b", "c", "d"))
  expect_equal(bound$z, c(NA, NA, TRUE, FALSE))

  mixed <- rbindfill(list(data.frame(v = 1:2), data.frame(v = c("x", "y"))), typeconflict = "coerce")
  expect_type(mixed$v, "character")
  expect_equal(mixed$v, c("1", "2", "x", "y"))

  tagged <- rbindfill(list(one = a[1, ], two = b[1, ]), id = "src")
  expect_equal(tagged$src, c("one", "two"))
  expect_equal(names(tagged)[[1]], "src")
})

test_that("composite key codec groups multi-column and mixed-type keys", {
  df <- data.frame(
    g1 = c("a", "a", "b", "a", "b"),
    g2 = c(1L, 2L, 1L, 1L, 1L),
    g3 = factor(c("x", "x", "y", "x", "y")),
    v = c(10, 20, 30, 40, 50),
    stringsAsFactors = FALSE
  )

  cnt <- count(df, by = c("g1", "g2", "g3"), sort = FALSE)
  expect_equal(nrow(cnt), 3L)
  expect_equal(cnt$n[cnt$g1 == "a" & cnt$g2 == 1L], 2L)

  agg <- aggregate(df, by = c("g1", "g3"), value = "v", fun = sum, sort = TRUE)
  expect_equal(agg$g1, c("a", "b"))
  expect_equal(agg$v, c(10 + 20 + 40, 30 + 50))

  u <- uniquerows(df, cols = c("g1", "g3"))
  expect_equal(nrow(u), 2L)

  dbl <- duplicaterows(df[c("g1", "g3")])
  expect_equal(dbl$g1, c("a", "a", "b", "a", "b"))
})

test_that("grouping a single numeric double key works through the codec", {
  df <- data.frame(k = c(1.5, 2.5, 1.5, 2.5, 1.5), v = 1:5)
  agg <- aggregate(df, by = "k", value = "v", fun = sum, sort = TRUE)
  expect_equal(agg$k, c(1.5, 2.5))
  expect_equal(agg$v, c(1 + 3 + 5, 2 + 4))
})

test_that("threaded and serial grouped aggregation agree", {
  skip_on_cran()
  set.seed(42)
  n <- 2e6
  d <- data.frame(g = sample(letters[1:6], n, TRUE), x = rnorm(n), stringsAsFactors = FALSE)

  old <- getOption("basetable.threads")
  on.exit(options(basetable.threads = old), add = TRUE)

  options(basetable.threads = 1L)
  s <- aggregate(d, by = "g", value = "x", fun = sum, sort = TRUE)
  m <- aggregate(d, by = "g", value = "x", fun = mean, sort = TRUE)
  v <- aggregate(d, by = "g", value = "x", fun = var, sort = TRUE)

  options(basetable.threads = 8L)
  s8 <- aggregate(d, by = "g", value = "x", fun = sum, sort = TRUE)
  m8 <- aggregate(d, by = "g", value = "x", fun = mean, sort = TRUE)
  v8 <- aggregate(d, by = "g", value = "x", fun = var, sort = TRUE)

  expect_equal(s8$x, s$x, tolerance = 1e-8)
  expect_equal(m8$x, m$x, tolerance = 1e-10)
  expect_equal(v8$x, v$x, tolerance = 1e-8)
  expect_equal(s8$g, s$g)
})

test_that("orderrows on character keys matches C-locale ordering", {
  set.seed(7)
  d <- data.frame(
    s = sample(c(letters, LETTERS), 5000, TRUE),
    k = sample(1:20, 5000, TRUE),
    stringsAsFactors = FALSE
  )
  d$s[sample(5000, 50)] <- NA

  out <- as.data.frame(orderrows(d, by = c("s", "k")))
  ref <- d[order(d$s, d$k, method = "radix", na.last = TRUE), ]
  rownames(out) <- NULL
  rownames(ref) <- NULL
  expect_equal(out, ref)
})

test_that("orderrows fast path handles character then double keys", {
  set.seed(8)
  d <- data.frame(
    s = sample(c(sprintf("k%03d", 1:40), NA), 10000, TRUE),
    x = sample(c(round(rnorm(9900), 2), rep(NA_real_, 100))),
    id = seq_len(10000),
    stringsAsFactors = FALSE
  )

  out <- as.data.frame(orderrows(d, by = c("s", "x")))
  ref <- d[order(d$s, d$x, method = "radix", na.last = TRUE), ]
  rownames(out) <- NULL
  rownames(ref) <- NULL
  expect_equal(out, ref)
})

test_that("windowed rangemerge matches a brute-force scan", {
  set.seed(11)
  ng <- 8
  x <- data.frame(k = sample(seq_len(ng), 120, TRUE), lo = round(rnorm(120), 1))
  x$hi <- x$lo + round(runif(120, 0, 4), 1)
  x$xid <- seq_len(120)
  y <- data.frame(
    k = sample(seq_len(ng), 200, TRUE),
    val = round(rnorm(200), 1),
    label = sprintf("L%03d", 1:200),
    stringsAsFactors = FALSE
  )

  got <- as.data.frame(rangemerge(x, y, by = "k", lower = "lo", upper = "hi", value = "val"))
  ref <- do.call(rbind, lapply(seq_len(nrow(x)), function(i) {
    h <- y[y$k == x$k[i] & y$val >= x$lo[i] & y$val <= x$hi[i], , drop = FALSE]
    if (!nrow(h)) data.frame(xid = i, label = NA_character_)
    else data.frame(xid = i, label = h$label)
  }))
  rownames(got) <- NULL
  rownames(ref) <- NULL
  expect_equal(got[c("xid", "label")], ref)
})

test_that("overlapmerge (two-column interval predicate) still works via the scan path", {
  x <- data.frame(id = c(1, 2), startx = c(1, 10), endx = c(4, 14))
  y <- data.frame(id = c(1, 2), starty = c(3, 12), endy = c(5, 15), value = c("a", "b"),
                  stringsAsFactors = FALSE)
  out <- overlapmerge(x, y, startx = "startx", endx = "endx",
                      starty = "starty", endy = "endy", by = "id")
  expect_equal(out$value, c("a", "b"))
})

test_that("threaded and single-threaded orderrows agree on a large frame", {
  skip_on_cran()
  set.seed(99)
  n <- 2e6
  d <- data.frame(
    s = sample(c(letters, LETTERS, month.name), n, TRUE),
    g = sample(1:5000, n, TRUE),
    r = round(rnorm(n), 3),
    stringsAsFactors = FALSE
  )
  d$s[sample(n, 200)] <- NA
  d$r[sample(n, 200)] <- NA

  old <- getOption("basetable.threads")
  on.exit(options(basetable.threads = old), add = TRUE)

  options(basetable.threads = 1L)
  a <- as.data.frame(orderrows(d, by = c("s", "g", "r")))
  options(basetable.threads = 8L)
  b <- as.data.frame(orderrows(d, by = c("s", "g", "r")))
  expect_identical(a, b)

  ref <- d[order(d$s, d$g, d$r, method = "radix", na.last = TRUE), ]
  rownames(a) <- NULL
  rownames(ref) <- NULL
  expect_equal(a, ref)
})

test_that("rbindfill bulk-copies matching columns and preserves shared class", {
  a <- data.frame(d = as.Date("2024-01-01") + 0:2, n = 1:3, s = c("a", "b", "c"),
                  stringsAsFactors = FALSE)
  b <- data.frame(d = as.Date("2024-02-01") + 0:1, n = 4:5, s = c("d", "e"),
                  stringsAsFactors = FALSE)

  r <- rbindfill(list(a, b))
  expect_s3_class(r, "basetable")
  expect_s3_class(r$d, "Date")
  expect_equal(r$d, c(a$d, b$d))
  expect_equal(r$n, c(1:3, 4:5))
  expect_equal(r$s, c("a", "b", "c", "d", "e"))

  # column present in only one frame -> NA fill; type promotion still applies
  x <- data.frame(k = 1:2, extra = c(TRUE, FALSE))
  y <- data.frame(k = c("3", "4"))
  rr <- as.data.frame(rbindfill(list(x, y), typeconflict = "coerce"))
  expect_equal(rr$k, c("1", "2", "3", "4"))
  expect_equal(rr$extra, c(TRUE, FALSE, NA, NA))
})

test_that("fused single-key aggregation matches the general path across cardinalities", {
  skip_on_cran()
  set.seed(5)
  for (k in c(3L, 200L, 5000L, 60000L)) {
    n <- 4e5
    d <- data.frame(
      g = sprintf("g%06d", sample(k, n, TRUE)),
      x = rnorm(n),
      stringsAsFactors = FALSE
    )
    d$x[sample(n, 50)] <- NA
    for (fn in c("sum", "mean", "sd", "n")) {
      old <- getOption("basetable.threads"); options(basetable.threads = 8L)
      got <- aggregate(d, by = "g", value = "x", fun = fn, sort = TRUE, na.rm = TRUE)
      options(basetable.threads = 1L)
      ref <- aggregate(d, by = "g", value = "x", fun = fn, sort = TRUE, na.rm = TRUE)
      options(basetable.threads = old)
      expect_equal(got$g, ref$g, info = paste(k, fn))
      expect_equal(got$x, ref$x, tolerance = 1e-8, info = paste(k, fn))
    }
  }
})

test_that("parallel membership probe agrees with serial (semi/anti/update)", {
  skip_on_cran()
  set.seed(11)
  n <- 1.2e6
  x <- data.frame(k = sample(sprintf("k%05d", 1:2000), n, TRUE),
                  v = seq_len(n), stringsAsFactors = FALSE)
  y <- data.frame(k = sprintf("k%05d", sample(2000, 900)), stringsAsFactors = FALSE)

  old <- getOption("basetable.threads"); on.exit(options(basetable.threads = old), add = TRUE)
  options(basetable.threads = 8L); a <- as.data.frame(semimerge(x, y, by = "k"))
  options(basetable.threads = 1L); b <- as.data.frame(semimerge(x, y, by = "k"))
  expect_identical(a, b)
  expect_equal(sort(unique(a$k)), sort(intersect(x$k, y$k)))
  expect_equal(nrow(a) + nrow(antimerge(x, y, by = "k")), n)
})

test_that("parallel equi-join probe and materialisation agree with serial", {
  skip_on_cran()
  set.seed(12)
  n <- 1.2e6
  x <- data.frame(k = sample(sprintf("k%05d", 1:2000), n, TRUE),
                  a = rnorm(n), lab = sample(letters, n, TRUE),
                  stringsAsFactors = FALSE)
  y <- data.frame(k = sprintf("k%05d", sample(2000, 1500)),
                  w = rnorm(1500), stringsAsFactors = FALSE)

  old <- getOption("basetable.threads"); on.exit(options(basetable.threads = old), add = TRUE)
  options(basetable.threads = 8L)
  i8 <- as.data.frame(merge(x, y, by = "k"))
  l8 <- as.data.frame(merge(x, y, by = "k", all.x = TRUE))
  options(basetable.threads = 1L)
  i1 <- as.data.frame(merge(x, y, by = "k"))
  l1 <- as.data.frame(merge(x, y, by = "k", all.x = TRUE))

  ord <- function(d) { d <- d[do.call(order, d), ]; rownames(d) <- NULL; d }
  expect_identical(ord(i8), ord(i1))
  expect_identical(ord(l8), ord(l1))
  expect_equal(nrow(l1), n)
  ref <- base::merge(x, y, by = "k")
  expect_equal(ord(i1)[names(ref)], ord(ref), ignore_attr = TRUE)
})

# ---- string kernel paths ---------------------------------------------------

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

test_that("string numeric ordering ranks missing and repeated groups stably", {
  old <- options("basetable.threads")
  on.exit(options(old), add = TRUE)
  set.seed(517)
  x <- data.frame(g = sample(c(NA_character_, "", letters), 220000L, TRUE),
                  x = sample(c(NA_real_, NaN, -Inf, -0, 0, 2, Inf),
                             220000L, TRUE), id = seq_len(220000L))
  expected <- x$id[order(x$g, x$x, method = "radix", na.last = TRUE)]
  for (threads in c(1L, 4L)) {
    options(basetable.threads = threads)
    expect_identical(orderrows(x, by = c("g", "x"))$id, expected)
    expect_identical(orderrows(x[FALSE, ], by = c("g", "x"))$id, integer())
  }
})

test_that("numeric prefix ordering refines close values without losing precision", {
  old <- options("basetable.threads")
  on.exit(options(old), add = TRUE)
  set.seed(910)
  for (n in c(50L, 500L, 10000L)) {
    # These values share their leading bits but differ near machine precision.
    values <- 1 + sample(0:100, n, TRUE) * .Machine$double.eps
    x <- data.frame(g = rep(c("a", "b"), length.out = n),
                    x = values, id = seq_len(n))
    ref <- order(x$g, x$x, method = "radix")
    for (threads in c(1L, 4L)) {
      options(basetable.threads = threads)
      out <- orderrows(x, by = c("g", "x"))
      expect_identical(out$id, x$id[ref])
      expect_identical(out$x, x$x[ref])
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
