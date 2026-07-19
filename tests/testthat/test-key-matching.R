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
