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

test_that("split, by_apply, and combine operate on ordinary frames", {
  pieces <- split(iris, by = "Species")
  expect_length(pieces, 3L)

  stats <- by_apply(
    iris,
    by = "Species",
    fun = function(d) data.frame(Species = d$Species[[1]], mean_sl = mean(d$Sepal.Length))
  )
  expect_length(stats, 3L)

  bound <- combine(stats)
  expect_equal(nrow(bound), 3L)
})
