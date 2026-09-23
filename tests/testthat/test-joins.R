# ---- merge -----------------------------------------------------------------

test_that("merge joins two tables", {
  x <- data.frame(id = c(1, 2), value_x = c("a", "b"))
  y <- data.frame(id = c(1, 3), value_y = c("c", "d"))

  out <- merge(x, y, by = "id", all.x = TRUE)

  expect_s3_class(out, "basetable")
  expect_equal(nrow(out), 2L)
  expect_true(all(c("id", "value_x", "value_y") %in% names(out)))
})

# ---- semimerge -------------------------------------------------------------

test_that("semimerge keeps matching rows from x", {
  x <- data.frame(id = c(1, 2, 3), value = c("a", "b", "c"))
  y <- data.frame(id = c(2, 3, 4), flag = c(TRUE, FALSE, TRUE))

  out <- semimerge(x, y, by = "id")

  expect_s3_class(out, "basetable")
  expect_equal(out$id, c(2, 3))
  expect_equal(out$value, c("b", "c"))
})

test_that("semimerge errors clearly on an empty by=", {
  x <- data.frame(id = 1:2)
  y <- data.frame(id = 1:2)
  expect_error(semimerge(x, y, by = character(0)), "must contain at least one column")
})

# ---- antimerge -------------------------------------------------------------

test_that("antimerge keeps non-matching rows from x", {
  x <- data.frame(id = c(1, 2, 3), value = c("a", "b", "c"))
  y <- data.frame(id = c(2, 4), flag = c(TRUE, TRUE))

  out <- antimerge(x, y, by = "id")

  expect_s3_class(out, "basetable")
  expect_equal(out$id, c(1, 3))
  expect_equal(out$value, c("a", "c"))
})

test_that("antimerge errors clearly on an empty by=", {
  x <- data.frame(id = 1:2)
  y <- data.frame(id = 1:2)
  expect_error(antimerge(x, y, by = character(0)), "must contain at least one column")
})

# ---- key matching ----------------------------------------------------------

test_that("matchedkeys and unmatchedkeys use joins correctly", {
  x <- data.frame(id = c(1, 2, 2, 3, 4), v = letters[1:5])
  y <- data.frame(id = c(2, 2, 5), w = c(10, 20, 30))

  matched <- matchedkeys(x, y, by = "id")
  expect_equal(sort(matched$v), c("b", "c"))

  unmatched <- unmatchedkeys(x, y, by = "id")
  expect_equal(sort(unmatched$id), c(1, 3, 4))
  expect_equal(nrow(unmatched), 3L)
})

test_that("antimerge and semimerge agree with matchedkeys/unmatchedkeys", {
  x <- data.frame(id = c(1, 2, 2, 3, 4), v = letters[1:5])
  y <- data.frame(id = c(2, 2, 5), w = c(10, 20, 30))

  expect_equal(sort(semimerge(x, y, by = "id")$v), sort(matchedkeys(x, y, by = "id")$v))
  expect_equal(sort(antimerge(x, y, by = "id")$id), c(1, 3, 4))
})

test_that("addedrows keeps only keys absent from old", {
  old <- data.frame(id = c(1, 2), v = c("a", "b"))
  new <- data.frame(id = c(1, 2, 3), v = c("a", "b", "c"))
  out <- addedrows(old, new, by = "id")
  expect_equal(out$id, 3)
})

# ---- set rows --------------------------------------------------------------

test_that("intersectrows keeps x rows whose key is present in y", {
  x <- data.frame(id = c(1, 2, 3), value = c("a", "b", "c"))
  y <- data.frame(id = c(2, 3, 4))

  out <- intersectrows(x, y, by = "id")
  expect_setequal(out$id, c(2, 3))
})

test_that("diffrows keeps x rows whose key is absent from y", {
  x <- data.frame(id = c(1, 2, 3), value = c("a", "b", "c"))
  y <- data.frame(id = c(2, 3, 4))

  out <- diffrows(x, y, by = "id")
  expect_equal(out$id, 1)
})

# ---- crossmerge ------------------------------------------------------------

test_that("crossmerge returns the Cartesian product", {
  x <- data.frame(id = c(1, 2))
  y <- data.frame(label = c("a", "b", "c"))

  out <- crossmerge(x, y)

  expect_s3_class(out, "basetable")
  expect_equal(nrow(out), 6L)
})

# ---- updatemerge -----------------------------------------------------------

test_that("updatemerge updates selected columns", {
  x <- data.frame(id = c(1, 2, 3), value = c("a", "b", "c"), other = c(1, 2, 3))
  y <- data.frame(id = c(2, 3), value = c("z", "y"))

  out <- updatemerge(x, y, by = "id", cols = "value")

  expect_s3_class(out, "basetable")
  expect_equal(out$value, c("a", "z", "y"))
  expect_equal(out$other, c(1, 2, 3))
})

# ---- nonequimerge ----------------------------------------------------------

test_that("nonequimerge still does a plain equi-merge when by has no conditions", {
  x <- data.frame(id = c(1, 2), value = c("a", "b"))
  y <- data.frame(id = c(1, 2), label = c("c", "d"))

  out <- nonequimerge(x, y, by = "id")

  expect_s3_class(out, "basetable")
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

# ---- rangemerge ------------------------------------------------------------

test_that("rangemerge keeps only y rows whose value falls within x's [lower, upper]", {
  x <- data.frame(id = c(1, 1, 2), lower = c(0, 0, 0), upper = c(10, 10, 20))
  y <- data.frame(id = c(1, 1, 2), val = c(5, 15, 5), label = c("a", "b", "c"))

  out <- rangemerge(x, y, by = "id", lower = "lower", upper = "upper", value = "val")

  expect_s3_class(out, "basetable")
  expect_equal(nrow(out), 3L)
  expect_equal(out$label, c("a", "a", "c"))
  expect_false("val_end" %in% names(out))
})

test_that("rangemerge keeps unmatched x rows with NA y columns", {
  x <- data.frame(id = 1, lower = 100, upper = 200)
  y <- data.frame(id = 1, val = c(5, 15), label = c("a", "b"))

  out <- rangemerge(x, y, by = "id", lower = "lower", upper = "upper", value = "val")

  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$label))
})

# ---- overlapmerge ----------------------------------------------------------

test_that("overlapmerge matches overlapping intervals", {
  x <- data.frame(id = c(1, 2), startx = c(1, 10), endx = c(4, 14))
  y <- data.frame(id = c(1, 2), starty = c(3, 12), endy = c(5, 15), value = c("a", "b"))

  out <- overlapmerge(x, y, startx = "startx", endx = "endx", starty = "starty", endy = "endy", by = "id")

  expect_s3_class(out, "basetable")
  expect_true("value" %in% names(out))
  expect_equal(out$value, c("a", "b"))
})

# ---- rollingmerge ----------------------------------------------------------

test_that("rollingmerge joins using a rolling key", {
  x <- data.frame(id = c(1, 1, 1), time = c(5, 10, 15))
  y <- data.frame(id = c(1, 1, 1), time = c(3, 8, 14), value = c("a", "b", "c"))

  out <- rollingmerge(x, y, by = c("id", "time"), direction = "backward")

  expect_s3_class(out, "basetable")
  expect_true("value" %in% names(out))
  expect_equal(out$value, c("a", "b", "c"))
})

# ---- nearestmerge ----------------------------------------------------------

test_that("nearestmerge wraps rollingmerge nearest mode", {
  x <- data.frame(id = c(1, 3), value = c("a", "b"))
  y <- data.frame(id = c(2, 4), label = c("c", "d"))

  out <- nearestmerge(x, y, by = "id")

  expect_s3_class(out, "basetable")
  expect_true("id" %in% names(out))
})
