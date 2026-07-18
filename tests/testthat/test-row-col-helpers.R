test_that("rowapply/rownumber basics", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_equal(rowapply(df, fun = sum), c(5, 7, 9))
  expect_equal(rownumber(c("a", "b", "c")), 1:3)
})

test_that("column metadata helpers", {
  df <- data.frame(a = 1:3, b = c("x", "x", "y"), stringsAsFactors = FALSE)

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
})
