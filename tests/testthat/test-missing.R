# ---- keepmissing -----------------------------------------------------------

test_that("keepmissing returns rows with missing values", {
  data <- data.frame(
    id = c(1, 2, 3),
    value = c("a", NA, " ")
  )

  out <- keepmissing(data)

  expect_s3_class(out, "basetable")
  expect_equal(out$id, c(2, 3))
})

# ---- missingrows -----------------------------------------------------------

test_that("missingrows returns rows with missing values", {
  data <- data.frame(
    id = c(1, 2, 3),
    value = c("a", NA, " ")
  )

  out_any <- missingrows(data)
  out_all <- missingrows(data, cols = "value", mode = "all")

  expect_s3_class(out_any, "basetable")
  expect_equal(out_any$id, c(2, 3))
  expect_equal(out_all$id, c(2, 3))
})

# ---- omitmissing -----------------------------------------------------------

test_that("omitmissing drops rows with missing values", {
  data <- data.frame(
    id = c(1, 2, 3),
    value = c("a", NA, " ")
  )

  out <- omitmissing(data)

  expect_s3_class(out, "basetable")
  expect_equal(out$id, 1)
})

# ---- missingindicator ------------------------------------------------------

test_that("missingindicator appends logical indicator columns", {
  data <- data.frame(
    id = c(1, 2, 3),
    value = c("a", NA, " ")
  )

  out <- missingindicator(data, cols = "value")

  expect_s3_class(out, "basetable")
  expect_true("missing_value" %in% names(out))
  expect_equal(out$missing_value, c(FALSE, TRUE, TRUE))
})

# ---- filldown --------------------------------------------------------------

test_that("filldown carries values forward within groups", {
  data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    visit = c(1, 2, 3, 1, 2),
    treatment = c("A", NA, NA, NA, "B")
  )

  out <- filldown(data, cols = "treatment", by = "id")

  expect_s3_class(out, "basetable")
  expect_equal(out$treatment, c("A", "A", "A", NA, "B"))
})

# ---- fillup ----------------------------------------------------------------

test_that("fillup carries values backward within groups", {
  data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    visit = c(1, 2, 3, 1, 2),
    treatment = c("A", NA, NA, NA, "B")
  )

  out <- fillup(data, cols = "treatment", by = "id")

  expect_s3_class(out, "basetable")
  expect_equal(out$treatment, c("A", NA, NA, "B", "B"))
})
