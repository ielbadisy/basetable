test_that("nonequimerge still does a plain equi-merge when by has no conditions", {
  x <- data.frame(id = c(1, 2), value = c("a", "b"))
  y <- data.frame(id = c(1, 2), label = c("c", "d"))

  out <- nonequimerge(x, y, by = "id")

  expect_s3_class(out, "data.table")
  expect_true("label" %in% names(out))
  expect_equal(out$label, c("c", "d"))
})

test_that("nonequimerge matches rows using inequality conditions", {
  x <- data.frame(id = 1, date = as.Date("2024-01-15"))
  y <- data.frame(
    id = 1,
    start_date = as.Date(c("2024-01-01", "2024-02-01")),
    end_date = as.Date(c("2024-01-31", "2024-02-28")),
    period = c("Jan", "Feb")
  )

  out <- nonequimerge(x, y, by = c("id", "date>=start_date", "date<=end_date"))

  expect_equal(nrow(out), 1L)
  expect_equal(out$period, "Jan")
})

test_that("nonequimerge drops x rows with no matching y row (inner join)", {
  x <- data.frame(id = 1, date = as.Date("2024-03-15"))
  y <- data.frame(
    id = 1,
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-01-31"),
    period = "Jan"
  )

  out <- nonequimerge(x, y, by = c("id", "date>=start_date", "date<=end_date"))

  expect_equal(nrow(out), 0L)
})

test_that("nonequimerge errors clearly on an empty by=", {
  x <- data.frame(id = 1:2)
  y <- data.frame(id = 1:2)
  expect_error(nonequimerge(x, y, by = character(0)), "must contain at least one column")
})
