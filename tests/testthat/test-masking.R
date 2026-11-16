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

test_that("subset(), split(), within(), stack(), unstack() keep base meaning off data frames", {
  v <- c(a = 1, b = 5, c = 9)
  expect_identical(subset(v, v > 2), base::subset(v, v > 2))
  m <- matrix(1:6, 2, dimnames = list(NULL, c("p", "q", "r")))
  expect_identical(subset(m, m[, "p"] > 1, select = q), base::subset(m, m[, "p"] > 1, select = q))
  expect_identical(subset(x = airquality, Temp > 90, select = Ozone),
                   base::subset(airquality, Temp > 90, select = Ozone))
  expect_identical(split(mtcars, mtcars$cyl), base::split(mtcars, mtcars$cyl))
  expect_identical(split(1:6, c(1, 2)), base::split(1:6, c(1, 2)))
  expect_identical(split(mtcars, list(mtcars$am, mtcars$vs), sep = "_"),
                   base::split(mtcars, list(mtcars$am, mtcars$vs), sep = "_"))
  expect_s3_class(split(mtcars, "cyl")[[1]], "basetable")
  l <- list(a = 1:3)
  expect_identical(within(l, b <- a * 2), base::within(l, b <- a * 2))
  sl <- list(a = 1:2, b = 3)
  expect_identical(stack(sl), utils::stack(sl))
  st <- utils::stack(list(a = 1:2, b = 3:4))
  expect_identical(unstack(x = st), utils::unstack(st))
})

test_that("truncate() on a connection and profile() on a model go to base/stats", {
  f <- tempfile(); con <- file(f, "w+"); on.exit(close(con))
  writeLines("abcdef", con)
  expect_silent(truncate(con))
  expect_identical(truncate("abcdefgh", 5), "ab...")
  skip_if(getRversion() < "4.4.0")  # profile.glm moved from MASS to stats in 4.4.0
  fit <- glm(am ~ wt, binomial, mtcars)
  expect_s3_class(profile(fit), "profile")
  expect_s3_class(profile(mtcars), "basetable")
})
