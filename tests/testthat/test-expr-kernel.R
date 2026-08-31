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

test_that("the single-comparison fast path matches the general kernel and eval()", {
  df <- data.frame(a = c(1, NA, 3, 4, 5), b = c(5, 4, NA, 2, 1))
  for (p in list(quote(a > b), quote(a >= 2), quote(3 < a), quote(a == b),
                 quote(a != b), quote(a <= b), quote(b > a))) {
    fast <- eval(bquote(basetable::subset(df, .(p))))
    r <- eval(p, df); r[is.na(r)] <- FALSE
    expect_equal(as.data.frame(fast), df[r, , drop = FALSE],
                 ignore_attr = TRUE, info = deparse(p))
  }
})

test_that("two-comparison AND fast path matches eval()", {
  df <- data.frame(a = c(1, NA, 3, 4, 5, 6), b = c(6, 5, NA, 3, 2, 1),
                   c = c(0, 1, 2, 3, 4, 5))
  for (p in list(quote(a > 2 & b < 5), quote(a >= b & c < 4),
                 quote(a < 5 & b > 1), quote(a != c & b >= 2),
                 quote(3 < a & c <= 3))) {
    fast <- eval(bquote(basetable::subset(df, .(p))))
    r <- eval(p, df); r[is.na(r)] <- FALSE
    expect_equal(as.data.frame(fast), df[r, , drop = FALSE],
                 ignore_attr = TRUE, info = deparse(p))
  }
})

test_that("parallel stream compaction in subset() is correct", {
  skip_on_cran()
  set.seed(3)
  n <- 1.5e6
  d <- data.frame(x = rnorm(n), i = sample(1:9, n, TRUE),
                  s = sample(letters, n, TRUE), stringsAsFactors = FALSE)
  old <- getOption("basetable.threads"); on.exit(options(basetable.threads = old), add = TRUE)
  options(basetable.threads = 8L)
  a <- as.data.frame(subset(d, x > 0.3))
  options(basetable.threads = 1L)
  b <- as.data.frame(subset(d, x > 0.3))
  expect_identical(a, b)
  m <- d$x > 0.3
  ref <- d[m, , drop = FALSE]; rownames(ref) <- NULL; rownames(a) <- NULL
  expect_equal(a, ref)
})

test_that("fused filter kernel: threaded and serial agree, and match eval()", {
  skip_on_cran()
  set.seed(11)
  n <- 1.2e6
  d <- data.frame(
    x = rnorm(n), y = rnorm(n),
    k = sample(1:5, n, TRUE),
    s = sample(letters, n, TRUE),
    stringsAsFactors = FALSE
  )
  d$x[sample(n, 1000)] <- NA
  old <- getOption("basetable.threads")
  on.exit(options(basetable.threads = old), add = TRUE)
  for (p in list(quote(x > 0.5), quote(x > 0 & y < 0), quote(0.2 < x & y <= 0.3))) {
    options(basetable.threads = 8L)
    a <- as.data.frame(eval(bquote(basetable::subset(d, .(p)))))
    options(basetable.threads = 1L)
    b <- as.data.frame(eval(bquote(basetable::subset(d, .(p)))))
    expect_identical(a, b, info = deparse(p))
    r <- eval(p, d); r[is.na(r)] <- FALSE
    ref <- d[r, , drop = FALSE]; rownames(ref) <- NULL
    expect_equal(a, ref, ignore_attr = TRUE, info = deparse(p))
  }
})

test_that("fused filter honours select and drops NA rows like base subset()", {
  set.seed(12)
  d <- data.frame(x = c(rnorm(50), NA, NA), g = sample(letters[1:3], 52, TRUE),
                  id = 1:52, stringsAsFactors = FALSE)
  out <- as.data.frame(basetable::subset(d, x > 0, select = c("g", "id")))
  ref <- d[d$x > 0 & !is.na(d$x), c("g", "id"), drop = FALSE]; rownames(ref) <- NULL
  expect_equal(out, ref, ignore_attr = TRUE)
})

test_that("fused filter handles integer and logical columns", {
  d <- data.frame(
    i = c(1L, 2L, NA_integer_, 4L, 5L),
    flag = c(TRUE, FALSE, NA, TRUE, FALSE),
    x = seq_len(5)
  )
  cases <- list(
    quote(i > 2L),
    quote(2L < i & i <= 5L),
    quote(flag == TRUE),
    quote(flag != FALSE & i >= 1L)
  )
  for (p in cases) {
    out <- as.data.frame(eval(bquote(basetable::subset(d, .(p)))))
    r <- eval(p, d); r[is.na(r)] <- FALSE
    ref <- d[r, , drop = FALSE]; rownames(ref) <- NULL
    expect_equal(out, ref, ignore_attr = TRUE, info = deparse(p))
  }
})

test_that("shapes outside the fused kernel still work via the mask path", {
  set.seed(13)
  d <- data.frame(x = rnorm(2000), y = rnorm(2000), i = sample(1:100, 2000, TRUE))
  cases <- list(quote(x > y), quote(x > 0 | y > 0), quote(abs(x) > 1))
  for (p in cases) {
    out <- as.data.frame(eval(bquote(basetable::subset(d, .(p)))))
    r <- eval(p, d); r[is.na(r)] <- FALSE
    ref <- d[r, , drop = FALSE]; rownames(ref) <- NULL
    expect_equal(out, ref, ignore_attr = TRUE, info = deparse(p))
  }
})
