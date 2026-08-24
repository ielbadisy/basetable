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

test_that("subset/pick/transform/reorder/renamecols compose into a pipeline", {
  out <- mtcars |>
    subset(cyl == 6 & mpg > 18) |>
    pick(c("mpg", "cyl", "hp")) |>
    transform(ratio = hp / mpg) |>
    reorder(by = "ratio", decreasing = TRUE)

  expect_s3_class(out, "data.table")
  expect_true(all(out$cyl == 6))
  expect_equal(names(out), c("mpg", "cyl", "hp", "ratio"))

  renamed <- renamecols(out, horsepower = hp)
  expect_true("horsepower" %in% names(renamed))

  compact <- transform(out, ratio2 = ratio * 2, .keep = FALSE)
  expect_equal(names(compact), "ratio2")
})

test_that("summaries, uniquerows, row slicing, move, and rbindfill work together", {
  stats <- summaries(mtcars, mean_mpg = mean(mpg), n = length(mpg), by = "cyl")
  expect_s3_class(stats, "data.table")
  expect_equal(nrow(stats), length(unique(mtcars$cyl)))
  expect_true(all(c("cyl", "mean_mpg", "n") %in% names(stats)))

  uniq <- uniquerows(mtcars, cols = "cyl")
  expect_equal(nrow(uniq), length(unique(mtcars$cyl)))

  two <- data.table::as.data.table(mtcars)[1:2]
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
  input <- data.table::data.table(
    id = c(1L, 1L, 2L),
    value = c(3, 3, 1)
  )
  original <- data.table::copy(input)

  subset(input, value > 1)
  renamecols(input, key = id)
  uniquerows(input)
  input[1:2]
  move(input, "value")
  rbindfill(input, input)

  expect_identical(input, original)
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
