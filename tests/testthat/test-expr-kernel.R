test_that("bt_compile_expr only accepts the supported grammar", {
  d <- data.frame(a = 1:3, b = 4:6, c = 7:9, s = letters[1:3])
  expect_null(bt_compile_expr(quote(sqrt(a)), d))    # unknown function
  expect_null(bt_compile_expr(quote(a + z), d))      # z not a column
  expect_null(bt_compile_expr(quote(a %in% b), d))   # unsupported op
  expect_null(bt_compile_expr(quote("x"), d))        # string literal
  expect_null(bt_compile_expr(quote(s == a), d))     # string column
  expect_type(bt_compile_expr(quote(a > 1 & b < c), d), "list")
  expect_type(bt_compile_expr(quote(ifelse(a > 0, b, -c)), d), "list")
})

test_that("subset() gives identical results through the native kernel and eval()", {
  df <- as.data.frame(iris)
  df$flag <- df$Sepal.Length > 5

  preds <- list(
    quote(Sepal.Length > 5),
    quote(Sepal.Length >= 5 & Petal.Width < 1),
    quote((Sepal.Length - Sepal.Width) / Petal.Length > 2),
    quote(Sepal.Length * 2 <= Petal.Length + 8),
    quote(!flag | Petal.Length > 4),
    quote(flag & !(Sepal.Width > 3)),
    quote(Sepal.Length %% 2 > 0.5),
    quote(ifelse(flag, Petal.Length, Petal.Width) > 3),
    quote(-Sepal.Width + Sepal.Length > 2)
  )

  for (p in preds) {
    kernel <- eval(bquote(subset(df, .(p))))
    r <- eval(p, df, parent.frame())
    r[is.na(r)] <- FALSE
    manual <- df[r, , drop = FALSE]
    expect_equal(as.data.frame(kernel), manual, ignore_attr = TRUE, info = deparse(p))
  }
})

test_that("kernel matches eval() on NA propagation and three-valued logic", {
  df <- data.frame(
    x = c(1, NA, 3, 4, NA),
    y = c(NA, 2, 3, NA, 5),
    b = c(TRUE, NA, FALSE, TRUE, NA)
  )
  for (p in list(quote(x > 2), quote(x > 2 & y < 4), quote(x > 2 | y < 4),
                 quote(b | x > 3), quote(b & y > 1), quote(!b))) {
    plan <- basetable:::bt_compile_expr(p, df)
    r_kernel <- .Call(basetable:::bt_expr_, df, plan$code, plan$args, plan$consts, FALSE)
    r_eval <- eval(p, df)
    expect_identical(r_kernel, r_eval, info = deparse(p))
  }
})

test_that("subset() still errors when the predicate is not logical", {
  df <- data.frame(x = 1:5)
  expect_error(subset(df, subset = x + 1), "logical vector")
})
