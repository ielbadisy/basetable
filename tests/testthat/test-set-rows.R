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
