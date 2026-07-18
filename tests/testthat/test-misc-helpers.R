test_that("applycols/convertcols/replacecols/replacewhere transform selected columns", {
  df <- data.frame(a = 1:3, b = 4:6)

  out <- applycols(df, "a", function(x) x * 10)
  expect_equal(out$a, c(10, 20, 30))
  expect_equal(out$b, df$b)

  out2 <- convertcols(df, "a", as.character)
  expect_equal(out2$a, c("1", "2", "3"))

  out3 <- replacecols(df, c("a", "b"), list(c(9, 9, 9), c(8, 8, 8)))
  expect_equal(out3$a, c(9, 9, 9))
  expect_equal(out3$b, c(8, 8, 8))

  out4 <- replacewhere(df, a > 1, "b", 0)
  expect_equal(out4$b, c(4, 0, 0))
})

test_that("applyby/recombine apply a function per group and stitch the results back", {
  df <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))

  out <- applyby(df, "g", nrow)
  expect_equal(unname(unlist(out)), c(2, 1))

  pieces <- list(data.frame(x = 1), data.frame(x = 2))
  combined <- recombine(pieces, id = "src")
  expect_equal(combined$x, c(1, 2))
  expect_true("src" %in% names(combined))
})

test_that("naif/nato/blanktona/natoblank/replacevalues recode values", {
  expect_equal(naif(c(1, 99, 2), 99), c(1, NA, 2))
  expect_equal(nato(c(1, NA, 2), 0), c(1, 0, 2))
  expect_equal(blanktona(c("a", "", " ", "b")), c("a", NA, NA, "b"))
  expect_equal(natoblank(c("a", NA, "b")), c("a", "", "b"))
  expect_equal(replacevalues(c("a", "b", "c"), c("a", "b"), c("A", "B")), c("A", "B", "c"))
})

test_that("tolong/towide/transpose reshape a table", {
  wide <- data.frame(id = 1:2, x = c(10, 20), y = c(30, 40))

  long <- tolong(wide, cols = c("x", "y"))
  expect_equal(nrow(long), 4L)
  expect_true(all(c("id", "variable", "value") %in% names(long)))

  back <- towide(long, names = "variable", values = "value", idcols = "id", fun = sum)
  expect_equal(sort(names(back)), sort(c("id", "x", "y")))
  expect_equal(back$x, wide$x)
  expect_equal(back$y, wide$y)

  t_out <- transpose(data.frame(a = 1:2, b = 3:4))
  expect_equal(dim(t_out), c(2L, 2L))
})

test_that("map/map_lgl/map_int/reduce/reorder work as expected", {
  expect_equal(map(1:3, function(x) x + 1), list(2, 3, 4))
  expect_equal(map_lgl(1:3, function(x) x > 1), c(FALSE, TRUE, TRUE))
  expect_equal(map_int(1:3, function(x) x * 2L), c(2L, 4L, 6L))

  df <- data.frame(x = c(3, 1, 2))
  expect_equal(reorder(df, "x")$x, c(1, 2, 3))
})
