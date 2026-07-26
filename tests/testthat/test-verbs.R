test_that("new verbs return data.tables and manipulate data", {
  out <- mtcars |>
    filter(cyl == 6, mpg > 18) |>
    select(c("mpg", "cyl", "hp")) |>
    mutate(ratio = hp / mpg) |>
    arrange("ratio", decreasing = TRUE)

  expect_s3_class(out, "data.table")
  expect_true(all(out$cyl == 6))
  expect_equal(names(out), c("mpg", "cyl", "hp", "ratio"))

  renamed <- rename(out, horsepower = hp)
  expect_true("horsepower" %in% names(renamed))

  compact <- transmute(out, ratio2 = ratio * 2)
  expect_equal(names(compact), "ratio2")
})

test_that("summarise, distinct, slice, relocate, and bind helpers work", {
  stats <- summarise(mtcars, mean_mpg = mean(mpg), n = length(mpg), by = "cyl")
  expect_s3_class(stats, "data.table")
  expect_equal(nrow(stats), length(unique(mtcars$cyl)))
  expect_true(all(c("cyl", "mean_mpg", "n") %in% names(stats)))
  expect_equal(summarize(mtcars, mean_mpg = mean(mpg), by = "cyl"), summarise(mtcars, mean_mpg = mean(mpg), by = "cyl"))

  uniq <- distinct(mtcars, cols = "cyl")
  expect_equal(nrow(uniq), length(unique(mtcars$cyl)))

  two <- slice(mtcars, 1:2)
  expect_equal(nrow(two), 2L)

  moved <- relocate(mtcars, "hp", .before = "mpg")
  expect_equal(names(moved)[[1]], "hp")

  row_bound <- bind_rows(list(a = two, b = two), id = "source")
  expect_equal(nrow(row_bound), 4L)
  expect_true("source" %in% names(row_bound))

  col_bound <- bind_cols(data.frame(a = 1:2), data.frame(b = 3:4))
  expect_equal(names(col_bound), c("a", "b"))
})

test_that("map, traverse, and fold helpers work", {
  expect_equal(map(1:3, function(x) x + 1), list(2, 3, 4))
  expect_equal(traverse(list(a = 1:3, b = 10:12), function(a, b) a + b), list(11, 13, 15))
  expect_equal(foldr(1:4, `+`), 10L)
})

test_that("setthreads forwards to data.table", {
  old <- data.table::getDTthreads()
  on.exit(data.table::setDTthreads(old), add = TRUE)

  new <- if (old == 1L) 0L else 1L
  data.table::setDTthreads(new)
  expected <- data.table::getDTthreads()
  data.table::setDTthreads(old)

  prev <- setthreads(new)

  expect_equal(prev, old)
  expect_equal(data.table::getDTthreads(), expected)
})
