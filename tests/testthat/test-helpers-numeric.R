# ---- numeric helpers -------------------------------------------------------

test_that("rescale/standardize/center/winsorize", {
  x <- c(1, 2, 3, 4, 5)

  expect_equal(rescale(x), c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(rescale(x, to = c(0, 10)), c(0, 2.5, 5, 7.5, 10))

  expect_equal(standardize(x), (x - mean(x)) / sd(x))
  expect_equal(center(x), x - mean(x))

  y <- c(1, 2, 3, 4, 100)
  out <- winsorize(y, probs = c(0.1, 0.9))
  expect_true(max(out) < 100)
  expect_equal(out[2:3], y[2:3])
})

test_that("quantilegroup bins into the requested number of groups", {
  x <- 1:100
  out <- quantilegroup(x, n = 4)
  expect_equal(nlevels(out), 4L)
})

test_that("percentchange/percentrank", {
  expect_equal(percentchange(c(100, 110, 99)), c(NA, 10, -10))

  x <- c(10, 20, 20, 30)
  out <- percentrank(x)
  expect_equal(out[1], 1 / 4)
  expect_equal(out[4], 1)
})

test_that("cumcount/cumedist/cumavg", {
  expect_equal(cumcount(c("a", "b", "c")), 1:3)
  expect_equal(cumedist(c("a", "a", "b")), cumsum(!duplicated(c("a", "a", "b"))) / 1:3)
  expect_equal(cumavg(c(1, 2, 3)), c(1, 1.5, 2))
})

test_that("denserank ranks by sorted value with no gaps, ties sharing a rank", {
  expect_equal(denserank(c(30, 10, 20, 10, 30)), c(3, 1, 2, 1, 3))
  expect_equal(denserank(c(NA, 1, 2)), c(NA, 1, 2))
})

test_that("difference/lagvalue/leadvalue", {
  expect_equal(difference(c(1, 3, 6)), c(NA, 2, 3))
  expect_equal(difference(c(1, 3, 6, 10), lag = 2), c(NA, NA, 5, 7))

  expect_equal(lagvalue(1:4), c(NA, 1, 2, 3))
  expect_equal(lagvalue(1:4, n = 2, default = 0), c(0, 0, 1, 2))
  expect_equal(leadvalue(1:4), c(2, 3, 4, NA))
})

test_that("propcount computes proportions overall and within a margin", {
  df <- data.frame(grp = c("a", "a", "b"), sub = c("x", "y", "x"), stringsAsFactors = FALSE)

  out <- propcount(df, by = "grp")
  expect_equal(sum(out$prop), 1)

  out_margin <- propcount(df, by = c("grp", "sub"), margin = "grp")
  a_rows <- out_margin[out_margin$grp == "a", ]
  expect_equal(sum(a_rows$prop), 1)
})

# ---- rolling window regressions --------------------------------------------

test_that("rollmean honors na.rm", {
  x <- c(1, NA, 3, 4, 5)
  out_no_narm <- rollmean(x, 3, na.rm = FALSE)
  out_narm <- rollmean(x, 3, na.rm = TRUE)

  expect_true(is.na(out_no_narm[3]))
  expect_equal(out_narm[3], mean(c(1, 3), na.rm = TRUE))
})

test_that("rollmean honors partial", {
  out_partial <- rollmean(1:5, 3, partial = TRUE)
  out_full <- rollmean(1:5, 3, partial = FALSE)

  expect_false(is.na(out_partial[1]))
  expect_true(is.na(out_full[1]))
  expect_true(is.na(out_full[2]))
  expect_equal(out_full[3:5], out_partial[3:5])
})

test_that("rollvar computes a real rolling variance (not routed through frollapply)", {
  out <- rollvar(1:5, 3)
  expect_equal(out[3:5], c(var(1:3), var(2:4), var(3:5)))
})

test_that("rollsum/rollmin/rollmax/rollmedian/rollsd/rollprod honor na.rm", {
  x <- c(1, NA, 3, 4)
  expect_false(is.na(rollsum(x, 2, na.rm = TRUE)[2]))
  expect_false(is.na(rollmin(x, 2, na.rm = TRUE)[2]))
  expect_false(is.na(rollmax(x, 2, na.rm = TRUE)[2]))
  expect_false(is.na(rollmedian(x, 2, na.rm = TRUE)[2]))
  expect_false(is.na(rollsd(c(1, NA, 3, 5), 2, na.rm = TRUE)[4]))
  expect_false(is.na(rollprod(x, 2, na.rm = TRUE)[2]))
})

test_that("rollapply honors partial", {
  out_partial <- rollapply(1:5, 3, sum, partial = TRUE)
  out_full <- rollapply(1:5, 3, sum, partial = FALSE)

  expect_false(is.na(out_partial[1]))
  expect_true(is.na(out_full[1]))
})

# ---- row primitives --------------------------------------------------------

test_that("rowmin and rowmax handle NA correctly", {
  df <- data.frame(a = c(1, NA, 3), b = c(2, 2, NA), c = c(3, 1, 1))
  expect_equal(rowmin(df, na.rm = TRUE), c(1, 1, 1))
  expect_equal(rowmax(df, na.rm = TRUE), c(3, 2, 3))
  expect_equal(rowmin(df, na.rm = FALSE), c(1, NA, NA))
  expect_equal(rowmax(df, na.rm = FALSE), c(3, NA, NA))
})

test_that("rowany and rowall handle NA correctly", {
  df <- data.frame(x = c(TRUE, FALSE, NA, TRUE, NA), y = c(FALSE, FALSE, NA, NA, TRUE))
  expect_equal(rowany(df, na.rm = TRUE), c(TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(rowany(df, na.rm = FALSE), c(TRUE, FALSE, NA, TRUE, TRUE))
  expect_equal(rowall(df, na.rm = TRUE), c(FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(rowall(df, na.rm = FALSE), c(FALSE, FALSE, NA, NA, NA))
})

test_that("rowcount counts matches per row", {
  df <- data.frame(a = c(1, NA, 3), b = c(3, 2, 3), c = c(3, 3, NA))
  expect_equal(rowcount(df, value = 3, na.rm = TRUE), c(2L, 1L, 2L))
  expect_equal(rowcount(df, value = 3, na.rm = FALSE), c(2L, NA_integer_, NA_integer_))
})

test_that("rowfirst and rowlast respect na.rm", {
  df <- data.frame(a = c(1, NA, NA), b = c(NA, 2, NA), c = c(3, 3, NA))
  expect_equal(rowfirst(df, na.rm = TRUE), c(1, 2, NA))
  expect_equal(rowfirst(df, na.rm = FALSE), c(1, NA, NA))
  expect_equal(rowlast(df, na.rm = TRUE), c(3, 3, NA))
  expect_equal(rowlast(df, na.rm = FALSE), c(3, 3, NA))
})

# ---- row vectorized --------------------------------------------------------

test_that("unite pastes columns and honors na.rm", {
  data <- data.frame(a = c("x", NA), b = c("y", "z"), stringsAsFactors = FALSE)

  out <- unite(data, column = "combo", cols = c("a", "b"))
  expect_equal(out$combo, c("x_y", "NA_z"))

  out_narm <- unite(data, column = "combo", cols = c("a", "b"), na.rm = TRUE)
  expect_equal(out_narm$combo, c("x_y", "z"))
})

test_that("separate splits a column into fixed-width parts", {
  data <- data.frame(col = c("a-b-c", "d-e"), stringsAsFactors = FALSE)
  out <- separate(data, column = "col", into = c("p1", "p2", "p3"), sep = "-")

  expect_equal(out$p1, c("a", "d"))
  expect_equal(out$p2, c("b", "e"))
  expect_equal(out$p3, c("c", NA_character_))
})

test_that("emptyrows keeps only fully blank rows", {
  data <- data.frame(a = c(NA, "x", ""), b = c(NA, "y", NA), stringsAsFactors = FALSE)
  out <- emptyrows(data)

  expect_equal(nrow(out), 2L)
  expect_equal(rownames(out), c("1", "3"))
})

test_that("nearesttext finds the closest string match", {
  expect_equal(nearesttext("hllo", c("hello", "world", "help")), "hello")
})

test_that("fillboth fills both directions within groups", {
  data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    treatment = c("A", NA, NA, NA, "B")
  )

  out <- fillboth(data, cols = "treatment", by = "id")
  expect_equal(out$treatment, c("A", "A", "A", "B", "B"))
})

# ---- row col helpers -------------------------------------------------------

test_that("rowapply/rownumber basics", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_equal(rowapply(df, fun = sum), c(5, 7, 9))
  expect_equal(rownumber(c("a", "b", "c")), 1:3)
})

test_that("column metadata helpers", {
  df <- data.frame(a = 1:3, b = c("x", "x", "y"), stringsAsFactors = FALSE)

  expect_equal(colnames(df), c("a", "b"))
  expect_equal(classes(df)$class, c("integer", "character"))
  expect_equal(unname(uniques(df)), c(3L, 2L))
  expect_equal(unname(cardinality(df, "b")), 2)
  expect_equal(unname(cardinality(df, "b", prop = TRUE)), 2 / 3)
  expect_equal(constants(data.frame(a = 1, b = 1:2)), "a")
  expect_equal(emptycols(data.frame(a = c(NA, NA), b = 1:2)), "a")
  expect_equal(emptycols(data.frame(a = c("", "  "), b = c("x", "y"), stringsAsFactors = FALSE)), "a")
})

test_that("row-level dedup/blank helpers", {
  df <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"), stringsAsFactors = FALSE)
  expect_equal(nrow(duplicaterows(df)), 2L)

  expect_equal(duplicatekeys(df, "a")$a, 1)
  expect_equal(duplicatenames(data.frame(a = 1, a = 2, check.names = FALSE)), "a")
})

test_that("commonnames finds shared column names", {
  expect_equal(commonnames(data.frame(a = 1, b = 2), data.frame(b = 1, c = 2)), "b")
})

test_that("name-cleaning helpers", {
  df <- data.frame(`a b` = 1, check.names = FALSE)
  expect_equal(names(cleannames(df)), "a_b")
  expect_equal(names(repairnames(df, method = "unique")), "a_b")

  messy <- data.frame(`First Name` = 1, `2nd_col` = 2, check.names = FALSE)
  expect_equal(names(cleannames(messy)), c("first_name", "x2nd_col"))
  expect_equal(names(repairnames(messy, method = "universal")), c("first_name", "x2nd_col"))

  df2 <- data.frame(a = 1, b = 2)
  expect_equal(names(renamewith(df2, "a", toupper)), c("A", "b"))
})

test_that("move/firstcols/lastcols reorder columns", {
  df <- data.frame(a = 1, b = 2, c = 3)

  expect_equal(names(move(df, "c")), c("c", "a", "b"))
  expect_equal(names(move(df, "a", after = "c")), c("b", "c", "a"))
  expect_equal(names(firstcols(df, "c")), c("c", "a", "b"))
  expect_equal(names(lastcols(df, "a")), c("b", "c", "a"))
})

test_that("firstrows/lastrows/samplerows/samplefrac/reverse/orderrows", {
  df <- data.frame(x = 1:5)

  expect_equal(firstrows(df, 2)$x, c(1, 2))
  expect_equal(lastrows(df, 2)$x, c(4, 5))
  expect_equal(nrow(samplerows(df, 3)), 3L)
  expect_equal(nrow(samplefrac(df, 0.4)), 2L)
  expect_equal(reverse(df)$x, rev(df$x))
  expect_equal(orderrows(df, "x", decreasing = TRUE)$x, rev(df$x))
})

test_that("samplerows/samplefrac with by sample within each group", {
  set.seed(1)
  d <- data.frame(g = rep(letters[1:3], c(4, 8, 12)), x = 1:24)

  s <- samplerows(d, 2, by = "g")
  expect_equal(as.integer(table(s$g)), c(2L, 2L, 2L))
  expect_false(is.unsorted(s$x))                 # original order preserved

  capped <- samplerows(d, 100, by = "g")        # n above a group size caps
  expect_equal(nrow(capped), nrow(d))

  f <- samplefrac(d, 0.5, by = "g")
  expect_equal(as.integer(table(f$g)), c(2L, 4L, 6L))

  expect_equal(nrow(samplerows(d, 5)), 5L)       # ungrouped unchanged
})

test_that("subset with by evaluates the predicate per group", {
  set.seed(1)
  d <- data.frame(g = rep(c("a", "b"), each = 20), x = rnorm(40), id = 1:40,
                  stringsAsFactors = FALSE)

  got <- as.data.frame(subset(d, x > mean(x), by = "g"))
  ref <- do.call(rbind, lapply(base::split(d, d$g),
                               function(s) s[s$x > mean(s$x), ]))
  ref <- ref[order(ref$id), ]
  rownames(got) <- rownames(ref) <- NULL
  expect_equal(got, ref, ignore_attr = TRUE)

  # select still applies alongside by
  got2 <- as.data.frame(subset(d, x > mean(x), select = c("g", "id"), by = "g"))
  expect_named(got2, c("g", "id"))

  # a non-grouped predicate gives the same rows as the ungrouped call
  expect_equal(
    as.data.frame(subset(d, x > 0, by = "g")),
    as.data.frame(subset(d, x > 0)),
    ignore_attr = TRUE
  )
})

test_that("orderrows supports a direction for each sort column", {
  df <- data.frame(
    group = c("b", "a", "b", "a"),
    score = c(1, 1, 2, 2),
    id = 1:4
  )

  out <- orderrows(
    df,
    by = c("group", "score"),
    decreasing = c(FALSE, TRUE)
  )

  expect_equal(out$id, c(4, 2, 3, 1))
  expect_s3_class(out, "basetable")
  expect_equal(df$id, 1:4)
})

test_that("firstby/lastby pick one row per group", {
  df <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))

  expect_equal(firstby(df, "g")$v, c(1, 3))
  expect_equal(lastby(df, "g")$v, c(2, 3))
})

test_that("removeduplicates honors by= and keep=", {
  df <- data.frame(id = c(1, 1, 2), v = c("a", "b", "c"), stringsAsFactors = FALSE)

  expect_equal(removeduplicates(df, by = "id", keep = "first")$v, c("a", "c"))
  expect_equal(removeduplicates(df, by = "id", keep = "last")$v, c("b", "c"))
  expect_equal(nrow(removeduplicates(df, by = "id", keep = "none")), 1L)
  expect_equal(nrow(removeduplicates(data.frame(x = c(1, 1, 2)))), 2L)
  expect_equal(removeduplicates(data.frame(x = c(1, 1, 2, 3, 3, 3)), keep = "none")$x, 2)
})
