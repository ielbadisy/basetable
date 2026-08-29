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
