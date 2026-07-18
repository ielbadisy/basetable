test_that("floordate/ceilingdate/rounddate actually round instead of no-op", {
  d <- as.Date(c("2024-03-15", "2024-01-31"))

  expect_equal(floordate(d, "month"), as.Date(c("2024-03-01", "2024-01-01")))
  expect_equal(floordate(d, "year"), as.Date(c("2024-01-01", "2024-01-01")))
  expect_equal(floordate(as.Date("2024-03-15"), "week"), as.Date("2024-03-11"))

  expect_equal(ceilingdate(d, "month"), as.Date(c("2024-04-01", "2024-02-01")))
  expect_equal(ceilingdate(as.Date("2024-03-15"), "year"), as.Date("2025-01-01"))

  expect_equal(rounddate(as.Date("2024-03-20"), "month"), as.Date("2024-04-01"))
  expect_equal(rounddate(as.Date("2024-03-10"), "month"), as.Date("2024-03-01"))
})

test_that("addmonths/addyears are vectorized and honor invalid=", {
  out <- addmonths(as.Date(c("2024-01-15", "2024-02-15")), 1)
  expect_equal(out, as.Date(c("2024-02-15", "2024-03-15")))

  expect_equal(addmonths(as.Date("2024-01-31"), 1, invalid = "previous"), as.Date("2024-02-29"))
  expect_equal(addmonths(as.Date("2024-01-31"), 1, invalid = "next"), as.Date("2024-03-02"))
  expect_equal(addmonths(as.Date("2024-01-31"), 1, invalid = "missing"), as.Date(NA))
  expect_error(addmonths(as.Date("2024-01-31"), 1, invalid = "error"), "invalid date")

  expect_equal(addyears(as.Date("2024-02-29"), 1, invalid = "previous"), as.Date("2025-02-28"))
})

test_that("datediff honors the units argument", {
  x <- as.Date("2024-01-03")
  y <- as.Date("2024-01-01")

  expect_equal(datediff(x, y, units = "days"), 2)
  expect_equal(datediff(x, y, units = "weeks"), 2 / 7)
})
