test_that("pick and drop work on data frames", {
  out <- pick(mtcars, c("mpg", "cyl"))
  expect_equal(names(out), c("mpg", "cyl"))

  out2 <- drop(mtcars, c("disp", "hp"))
  expect_false(any(c("disp", "hp") %in% names(out2)))
})

test_that("subset and transform support nested and pipe style", {
  nested <- transform(subset(mtcars, cyl == 6, select = c("mpg", "hp", "wt")), ratio = hp / wt)
  piped <- mtcars |>
    subset(cyl == 6, select = c("mpg", "hp", "wt")) |>
    transform(ratio = hp / wt)

  expect_equal(nested, piped)
})

test_that("subset supports base-style negative bare-symbol select", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_equal(names(subset(df, select = -a)), c("b", "c"))
  expect_equal(names(subset(df, select = -c(a, b))), "c")
  expect_equal(names(subset(df, select = c(a, c))), c("a", "c"))

  keep <- c("b", "c")
  expect_equal(names(subset(df, select = keep)), keep)
})

test_that("aggregate and count summarize by groups", {
  agg <- aggregate(mtcars, by = "cyl", value = c("mpg", "hp"), fun = mean)
  expect_true(all(c("cyl", "mpg", "hp") %in% names(agg)))

  cnt <- count(mtcars, by = "cyl")
  expect_equal(sum(cnt$n), nrow(mtcars))
})

test_that("merge and key helpers behave consistently", {
  x <- data.frame(id = c(1, 2), value_x = c("a", "b"))
  y <- data.frame(id = c(1, 3), value_y = c("c", "d"))

  out <- merge(x, y, by = "id", all.x = TRUE)
  expect_equal(nrow(out), 2L)
  expect_equal(common_names(x, y), "id")
})

test_that("subset/pick/transform/orderrows/renamecols compose into a pipeline", {
  out <- mtcars |>
    subset(cyl == 6 & mpg > 18) |>
    pick(c("mpg", "cyl", "hp")) |>
    transform(ratio = hp / mpg) |>
    orderrows(by = "ratio", decreasing = TRUE)

  expect_s3_class(out, "basetable")
  expect_true(all(out$cyl == 6))
  expect_equal(names(out), c("mpg", "cyl", "hp", "ratio"))

  renamed <- renamecols(out, horsepower = hp)
  expect_true("horsepower" %in% names(renamed))

  compact <- transform(out, ratio2 = ratio * 2, .keep = FALSE)
  expect_equal(names(compact), "ratio2")
})

test_that("summaries, uniquerows, row slicing, move, and rbindfill work together", {
  stats <- summaries(mtcars, mean_mpg = mean(mpg), n = length(mpg), by = "cyl")
  expect_s3_class(stats, "basetable")
  expect_equal(nrow(stats), length(unique(mtcars$cyl)))
  expect_true(all(c("cyl", "mean_mpg", "n") %in% names(stats)))

  uniq <- uniquerows(mtcars, cols = "cyl")
  expect_equal(nrow(uniq), length(unique(mtcars$cyl)))

  two <- firstrows(mtcars, 2)
  expect_equal(nrow(two), 2L)

  moved <- move(mtcars, "hp", before = "mpg")
  expect_equal(names(moved)[[1]], "hp")

  row_bound <- rbindfill(list(a = two, b = two), id = "source")
  expect_equal(nrow(row_bound), 4L)
  expect_true("source" %in% names(row_bound))

  col_bound <- cbind(data.frame(a = 1:2), data.frame(b = 3:4))
  expect_equal(names(col_bound), c("a", "b"))
})

test_that("core verbs do not mutate their input", {
  input <- data.frame(
    id = c(1L, 1L, 2L),
    value = c(3, 3, 1)
  )
  original <- input

  subset(input, value > 1)
  renamecols(input, key = id)
  uniquerows(input)
  input[1:2]
  move(input, "value")
  rbindfill(input, input)

  expect_identical(input, original)
})

test_that("setthreads controls basetable defaults", {
  old <- getOption("basetable.threads", bt_default_threads())
  on.exit(options(basetable.threads = old), add = TRUE)

  prev <- setthreads(1L)

  expect_equal(prev, old)
  expect_equal(bt_default_threads(), 1L)
})

test_that("split and applyby operate on ordinary frames", {
  pieces <- split(iris, by = "Species")
  expect_length(pieces, 3L)

  stats <- applyby(
    iris,
    by = "Species",
    fun = function(d) data.frame(Species = d$Species[[1]], mean_sl = mean(d$Sepal.Length))
  )
  expect_length(stats, 3L)
})
