test_that("rollmean honors na.rm", {
  x <- c(1, NA, 3, 4, 5)
  out_no_narm <- rollmean(x, 3, na.rm = FALSE)
  out_narm <- rollmean(x, 3, na.rm = TRUE)

  expect_true(is.na(out_no_narm[3]))
  expect_equal(out_narm[3], mean(c(1, 3), na.rm = TRUE))
})

test_that("rollmean honors partial", {
  out_partial <- rollmean(1:5, 3, partial = TRUE)
  out_full <- rollmean(1:5, 3, partial = FALSE)

  expect_false(is.na(out_partial[1]))
  expect_true(is.na(out_full[1]))
  expect_true(is.na(out_full[2]))
  expect_equal(out_full[3:5], out_partial[3:5])
})

test_that("rollvar computes a real rolling variance (not routed through frollapply)", {
  out <- rollvar(1:5, 3)
  expect_equal(out[3:5], c(var(1:3), var(2:4), var(3:5)))
})

test_that("rollsum/rollmin/rollmax/rollmedian/rollsd/rollprod honor na.rm", {
  x <- c(1, NA, 3, 4)
  expect_false(is.na(rollsum(x, 2, na.rm = TRUE)[2]))
  expect_false(is.na(rollmin(x, 2, na.rm = TRUE)[2]))
  expect_false(is.na(rollmax(x, 2, na.rm = TRUE)[2]))
  expect_false(is.na(rollmedian(x, 2, na.rm = TRUE)[2]))
  expect_false(is.na(rollsd(c(1, NA, 3, 5), 2, na.rm = TRUE)[4]))
  expect_false(is.na(rollprod(x, 2, na.rm = TRUE)[2]))
})

test_that("rollapply honors partial", {
  out_partial <- rollapply(1:5, 3, sum, partial = TRUE)
  out_full <- rollapply(1:5, 3, sum, partial = FALSE)

  expect_false(is.na(out_partial[1]))
  expect_true(is.na(out_full[1]))
})
