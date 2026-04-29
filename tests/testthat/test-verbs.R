test_that("new verbs return tibbles and manipulate data", {
  out <- mtcars |>
    filter(cyl == 6, mpg > 18) |>
    select(c("mpg", "cyl", "hp")) |>
    mutate(ratio = hp / mpg) |>
    arrange("ratio", decreasing = TRUE)

  expect_s3_class(out, "tbl_df")
  expect_true(all(out$cyl == 6))
  expect_equal(names(out), c("mpg", "cyl", "hp", "ratio"))

  renamed <- rename(out, horsepower = hp)
  expect_true("horsepower" %in% names(renamed))

  compact <- transmute(out, ratio2 = ratio * 2)
  expect_equal(names(compact), "ratio2")
})

test_that("summarise, distinct, slice, relocate, and bind helpers work", {
  stats <- summarise(mtcars, mean_mpg = mean(mpg), n = length(mpg), by = "cyl")
  expect_s3_class(stats, "tbl_df")
  expect_equal(nrow(stats), length(unique(mtcars$cyl)))
  expect_true(all(c("cyl", "mean_mpg", "n") %in% names(stats)))

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

test_that("functionals-backed map helpers work", {
  expect_equal(map_dbl(1:3, function(x) x + 0.5), c(1.5, 2.5, 3.5))
  expect_equal(map_chr(1:2, as.character), c("1", "2"))
  expect_equal(reduce(1:4, `+`), 10L)

  seen <- character()
  out <- walk(c("a", "b"), function(x) seen <<- c(seen, x))
  expect_equal(out, c("a", "b"))
  expect_equal(seen, c("a", "b"))
})
