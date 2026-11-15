# basetable masks base::drop(), base::merge() and stats::aggregate() when
# attached. Calls written for the base/stats versions must still work.

test_that("drop() without cols is base::drop()", {
  m <- matrix(1:3, nrow = 1)
  expect_identical(drop(m), base::drop(m))
  a <- array(1:6, dim = c(1, 2, 3))
  expect_identical(drop(a), base::drop(a))
  expect_identical(names(drop(mtcars, "mpg")), setdiff(names(mtcars), "mpg"))
})

test_that("merge() with base-only arguments is base::merge()", {
  a <- data.frame(k = c(3, 1, 2), u = c("c", "a", "b"))
  b <- data.frame(key = c(1, 2, 4), v = c(10, 20, 40))
  expect_identical(
    merge(a, b, by.x = "k", by.y = "key", all = TRUE),
    base::merge(a, b, by.x = "k", by.y = "key", all = TRUE)
  )
  bb <- data.frame(k = c(1, 2, 4), v = c(10, 20, 40))
  expect_identical(
    merge(a, bb, by = "k", no.dups = TRUE),
    base::merge(a, bb, by = "k", no.dups = TRUE)
  )
  # basetable behaviour unchanged without base-only arguments
  out <- merge(a, bb, by = "k")
  expect_s3_class(out, "basetable")
  expect_identical(out$k, c(1, 2))
})

test_that("aggregate() calls written for stats::aggregate() go to stats", {
  expect_identical(
    aggregate(mpg ~ cyl, data = mtcars, FUN = mean),
    stats::aggregate(mpg ~ cyl, data = mtcars, FUN = mean)
  )
  expect_identical(
    aggregate(cbind(mpg, hp) ~ cyl + am, mtcars, mean),
    stats::aggregate(cbind(mpg, hp) ~ cyl + am, mtcars, mean)
  )
  expect_identical(
    aggregate(mtcars$mpg, list(cyl = mtcars$cyl), mean),
    stats::aggregate(mtcars$mpg, list(cyl = mtcars$cyl), mean)
  )
  expect_identical(
    aggregate(mtcars[c("mpg", "hp")], by = list(cyl = mtcars$cyl), FUN = median),
    stats::aggregate(mtcars[c("mpg", "hp")], by = list(cyl = mtcars$cyl), FUN = median)
  )
  expect_identical(
    aggregate(x = mtcars["mpg"], by = list(mtcars$am), FUN = max),
    stats::aggregate(x = mtcars["mpg"], by = list(mtcars$am), FUN = max)
  )
  expect_identical(
    aggregate(ldeaths, nfrequency = 1, FUN = sum),
    stats::aggregate(ldeaths, nfrequency = 1, FUN = sum)
  )
  # basetable behaviour unchanged
  out <- aggregate(mtcars, by = "cyl", value = "mpg", fun = mean)
  expect_s3_class(out, "basetable")
  expect_equal(out$mpg, as.numeric(tapply(mtcars$mpg, mtcars$cyl, mean)), ignore_attr = TRUE)
})
