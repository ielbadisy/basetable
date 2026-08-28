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
