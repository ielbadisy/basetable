test_that("unite pastes columns and honors na.rm", {
  data <- data.frame(a = c("x", NA), b = c("y", "z"), stringsAsFactors = FALSE)

  out <- unite(data, column = "combo", cols = c("a", "b"))
  expect_equal(out$combo, c("x_y", "NA_z"))

  out_narm <- unite(data, column = "combo", cols = c("a", "b"), na.rm = TRUE)
  expect_equal(out_narm$combo, c("x_y", "z"))
})

test_that("separate splits a column into fixed-width parts", {
  data <- data.frame(col = c("a-b-c", "d-e"), stringsAsFactors = FALSE)
  out <- separate(data, column = "col", into = c("p1", "p2", "p3"), sep = "-")

  expect_equal(out$p1, c("a", "d"))
  expect_equal(out$p2, c("b", "e"))
  expect_equal(out$p3, c("c", NA_character_))
})

test_that("emptyrows keeps only fully blank rows", {
  data <- data.frame(a = c(NA, "x", ""), b = c(NA, "y", NA), stringsAsFactors = FALSE)
  out <- emptyrows(data)

  expect_equal(nrow(out), 2L)
  expect_equal(rownames(out), c("1", "3"))
})

test_that("nearesttext finds the closest string match", {
  expect_equal(nearesttext("hllo", c("hello", "world", "help")), "hello")
})

test_that("fillboth fills both directions within groups", {
  data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    treatment = c("A", NA, NA, NA, "B")
  )

  out <- fillboth(data, cols = "treatment", by = "id")
  expect_equal(out$treatment, c("A", "A", "A", "B", "B"))
})
