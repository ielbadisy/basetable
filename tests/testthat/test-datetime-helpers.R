test_that("date component extractors match base R", {
  d <- as.Date(c("2024-03-15", "2024-12-31"))

  expect_equal(year(d), c(2024L, 2024L))
  expect_equal(month(d), c(3L, 12L))
  expect_equal(day(d), c(15L, 31L))
  expect_equal(weekday(d), weekdays(d))
  expect_equal(yearday(d), as.integer(format(d, "%j")))
  expect_equal(week(d), as.integer(format(d, "%U")))
  expect_equal(quarter(d), c(1L, 4L))
})

test_that("time component extractors work on POSIXct", {
  t <- as.POSIXct("2024-03-15 08:30:45", tz = "UTC")

  expect_equal(hour(t), 8L)
  expect_equal(minute(t), 30L)
  expect_equal(second(t), 45L)
})

test_that("adddays/addweeks shift dates by the expected amount", {
  d <- as.Date("2024-01-01")

  expect_equal(adddays(d, 10), as.Date("2024-01-11"))
  expect_equal(addweeks(d, 2), as.Date("2024-01-15"))
  expect_equal(adddays(d, -1), as.Date("2023-12-31"))
})

test_that("dateseq builds a sequence between two dates", {
  out <- dateseq(as.Date("2024-01-01"), as.Date("2024-01-05"))
  expect_equal(out, seq.Date(as.Date("2024-01-01"), as.Date("2024-01-05"), by = "day"))

  out_len <- dateseq(as.Date("2024-01-01"), as.Date("2024-02-01"), length.out = 5)
  expect_equal(length(out_len), 5L)
})

test_that("betweendates is inclusive on both ends", {
  x <- as.Date(c("2024-01-01", "2024-01-05", "2024-01-10", "2024-01-15"))
  out <- betweendates(x, "2024-01-05", "2024-01-10")

  expect_equal(out, c(FALSE, TRUE, TRUE, FALSE))
})
