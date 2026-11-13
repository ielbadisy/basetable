test_that("deprecated aliases warn at top level only", {
  df <- data.frame(a = 1:2)
  expect_warning(eval(bquote(basetable::as_basetable(.(df))), globalenv()), "asbasetable")
  pkgfun <- function(x) basetable::as_basetable(x)
  environment(pkgfun) <- asNamespace("stats")
  expect_silent(pkgfun(df))
  expect_true(isbasetable(pkgfun(df)))
})
