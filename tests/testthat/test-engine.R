test_that("native engine powers projection and row filtering", {
  df <- data.frame(a = 1:5, b = letters[1:5], c = c(TRUE, FALSE, TRUE, NA, TRUE))

  out <- pick(df, c("b", "a"))
  expect_s3_class(out, "data.table")
  expect_equal(out, data.table::as.data.table(df[c("b", "a")]))

  filtered <- subset(df, c, select = c(a, b))
  expect_equal(filtered, data.table::as.data.table(df[c(1L, 3L, 5L), c("a", "b")]))
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
  input <- data.table::data.table(g = c(2L, 1L, 2L), x = c(3, 2, 1))
  original <- data.table::copy(input)

  out <- orderrows(input, by = c("g", "x"), decreasing = c(FALSE, TRUE))

  expect_equal(out$g, c(1L, 2L, 2L))
  expect_equal(out$x, c(2, 3, 1))
  expect_identical(input, original)
})
