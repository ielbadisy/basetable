test_that("equaldata compares full table content", {
  x <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  y <- data.frame(a = 3:1, b = c("z", "y", "x"), stringsAsFactors = FALSE)

  expect_false(equaldata(x, y))
  expect_true(equaldata(x, y, ignoreorder = TRUE))
})

test_that("equalrows compares rows by key, ignoring order but not duplicates", {
  x <- data.frame(id = c(1, 2, 3))
  y_reordered <- data.frame(id = c(3, 2, 1))
  y_extra_dup <- data.frame(id = c(1, 2, 3, 3))
  y_different <- data.frame(id = c(4, 5, 6))

  expect_true(equalrows(x, y_reordered, by = "id"))
  expect_false(equalrows(x, y_extra_dup, by = "id"))
  expect_false(equalrows(x, y_different, by = "id"))
})

test_that("equalrows compares full row content, not just the key columns", {
  same_v <- data.frame(id = 1:3, v = c("a", "b", "c"))
  different_v <- data.frame(id = 1:3, v = c("a", "b", "ZZZZZ"))

  expect_true(equalrows(same_v, same_v, by = "id"))
  expect_false(equalrows(same_v, different_v, by = "id"))
})

test_that("sameschema/compareschema report column name and type differences", {
  x <- data.frame(a = 1L, b = "x", stringsAsFactors = FALSE)
  y <- data.frame(a = 1L, b = "x", stringsAsFactors = FALSE)
  z <- data.frame(a = 1L, c = 2, stringsAsFactors = FALSE)

  expect_true(sameschema(x, y))
  expect_false(sameschema(x, z))

  out <- compareschema(x, z)
  expect_equal(sort(out$column), c("a", "b", "c"))
  expect_false(out$in_y[out$column == "b"])
  expect_false(out$in_x[out$column == "c"])

  expect_equal(changedcols(x, z), compareschema(x, z))
})

test_that("joinrelationship classifies key cardinality", {
  one <- data.frame(id = c(1, 2, 3))
  dup <- data.frame(id = c(1, 1, 2))

  expect_equal(joinrelationship(one, one, "id"), "one-to-one")
  expect_equal(joinrelationship(one, dup, "id"), "one-to-many")
  expect_equal(joinrelationship(dup, one, "id"), "many-to-one")
  expect_equal(joinrelationship(dup, dup, "id"), "many-to-many")
})

test_that("unionrows/rbindfill combine rows", {
  x <- data.frame(id = c(1, 2))
  y <- data.frame(id = c(2, 3))

  out <- unionrows(x, y)
  expect_equal(sort(out$id), c(1, 2, 3))

  out_fill <- rbindfill(data.frame(a = 1), data.frame(a = 2, b = 3))
  expect_equal(out_fill$a, c(1, 2))
  expect_equal(out_fill$b, c(NA, 3))
})

test_that("addedrows/removedrows/changedrows track differences between two snapshots", {
  old <- data.frame(id = c(1, 2, 3), v = c("a", "b", "c"), stringsAsFactors = FALSE)
  new <- data.frame(id = c(2, 3, 4), v = c("b", "c2", "d"), stringsAsFactors = FALSE)

  expect_equal(addedrows(old, new, by = "id")$id, 4)
  expect_equal(removedrows(old, new, by = "id")$id, 1)

  changed <- changedrows(old, new, by = "id")
  expect_equal(changed$id, 3)
})
