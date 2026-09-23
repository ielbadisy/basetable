# ---- subset and transform basics -------------------------------------------

test_that("pick and drop work on data frames", {
  out <- pick(mtcars, c("mpg", "cyl"))
  expect_equal(names(out), c("mpg", "cyl"))

  out2 <- drop(mtcars, c("disp", "hp"))
  expect_false(any(c("disp", "hp") %in% names(out2)))
})

test_that("subset and transform support nested and pipe style", {
  nested <- transform(subset(mtcars, cyl == 6, select = c("mpg", "hp", "wt")), ratio = hp / wt)
  piped <- mtcars |>
    subset(cyl == 6, select = c("mpg", "hp", "wt")) |>
    transform(ratio = hp / wt)

  expect_equal(nested, piped)
})

test_that("subset supports base-style negative bare-symbol select", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_equal(names(subset(df, select = -a)), c("b", "c"))
  expect_equal(names(subset(df, select = -c(a, b))), "c")
  expect_equal(names(subset(df, select = c(a, c))), c("a", "c"))

  keep <- c("b", "c")
  expect_equal(names(subset(df, select = keep)), keep)
})

test_that("aggregate and count summarize by groups", {
  agg <- aggregate(mtcars, by = "cyl", value = c("mpg", "hp"), fun = mean)
  expect_true(all(c("cyl", "mpg", "hp") %in% names(agg)))

  cnt <- count(mtcars, by = "cyl")
  expect_equal(sum(cnt$n), nrow(mtcars))
})

test_that("merge and key helpers behave consistently", {
  x <- data.frame(id = c(1, 2), value_x = c("a", "b"))
  y <- data.frame(id = c(1, 3), value_y = c("c", "d"))

  out <- merge(x, y, by = "id", all.x = TRUE)
  expect_equal(nrow(out), 2L)
  expect_equal(common_names(x, y), "id")
})

test_that("subset/pick/transform/orderrows/renamecols compose into a pipeline", {
  out <- mtcars |>
    subset(cyl == 6 & mpg > 18) |>
    pick(c("mpg", "cyl", "hp")) |>
    transform(ratio = hp / mpg) |>
    orderrows(by = "ratio", decreasing = TRUE)

  expect_s3_class(out, "basetable")
  expect_true(all(out$cyl == 6))
  expect_equal(names(out), c("mpg", "cyl", "hp", "ratio"))

  renamed <- renamecols(out, horsepower = hp)
  expect_true("horsepower" %in% names(renamed))

  compact <- transform(out, ratio2 = ratio * 2, .keep = FALSE)
  expect_equal(names(compact), "ratio2")
})

test_that("summaries, uniquerows, row slicing, move, and rbindfill work together", {
  stats <- summaries(mtcars, mean_mpg = mean(mpg), n = length(mpg), by = "cyl")
  expect_s3_class(stats, "basetable")
  expect_equal(nrow(stats), length(unique(mtcars$cyl)))
  expect_true(all(c("cyl", "mean_mpg", "n") %in% names(stats)))

  uniq <- uniquerows(mtcars, cols = "cyl")
  expect_equal(nrow(uniq), length(unique(mtcars$cyl)))

  two <- firstrows(mtcars, 2)
  expect_equal(nrow(two), 2L)

  moved <- move(mtcars, "hp", before = "mpg")
  expect_equal(names(moved)[[1]], "hp")

  row_bound <- rbindfill(list(a = two, b = two), id = "source")
  expect_equal(nrow(row_bound), 4L)
  expect_true("source" %in% names(row_bound))

  col_bound <- cbind(data.frame(a = 1:2), data.frame(b = 3:4))
  expect_equal(names(col_bound), c("a", "b"))
})

test_that("core verbs do not mutate their input", {
  input <- data.frame(
    id = c(1L, 1L, 2L),
    value = c(3, 3, 1)
  )
  original <- input

  subset(input, value > 1)
  renamecols(input, key = id)
  uniquerows(input)
  input[1:2]
  move(input, "value")
  rbindfill(input, input)

  expect_identical(input, original)
})

test_that("setthreads controls basetable defaults", {
  old <- getOption("basetable.threads", bt_default_threads())
  on.exit(options(basetable.threads = old), add = TRUE)

  prev <- setthreads(1L)

  expect_equal(prev, old)
  expect_equal(bt_default_threads(), 1L)
})

test_that("split and applyby operate on ordinary frames", {
  pieces <- split(iris, by = "Species")
  expect_length(pieces, 3L)

  stats <- applyby(
    iris,
    by = "Species",
    fun = function(d) data.frame(Species = d$Species[[1]], mean_sl = mean(d$Sepal.Length))
  )
  expect_length(stats, 3L)
})

# ---- transform -------------------------------------------------------------

test_that("transform adds derived columns", {
  out <- transform(mtcars, ratio = mpg / cyl)

  expect_s3_class(out, "basetable")
  expect_true("ratio" %in% names(out))
  expect_equal(out$ratio[[1]], mtcars$mpg[[1]] / mtcars$cyl[[1]])
})

test_that("transform() with .keep = FALSE keeps only new columns", {
  out <- transform(mtcars, ratio = mpg / cyl, .keep = FALSE)

  expect_s3_class(out, "basetable")
  expect_equal(names(out), "ratio")
})

test_that("transform resolves values from its caller, including nested and .keep = FALSE calls", {
  data <- data.frame(value = 1:3)

  run_transform <- function(x) {
    multiplier <- 3
    transform(x, result = value * multiplier)
  }
  run_transform_keep <- function(x) {
    offset <- 2
    transform(x, result = value + offset)
  }
  run_transform_drop <- function(x) {
    denominator <- 2
    transform(x, result = value / denominator, .keep = FALSE)
  }

  expect_equal(run_transform(data)$result, c(3, 6, 9))
  expect_equal(run_transform_keep(data)$result, c(3, 4, 5))
  expect_equal(run_transform_drop(data)$result, c(0.5, 1, 1.5))
})

test_that("transform does not confuse an evaluated result with a value column", {
  data <- data.frame(value = 1:3)

  expect_equal(transform(data, doubled = value * 2)$doubled, c(2, 4, 6))
})

test_that("transform(by =) computes expressions within each group", {
  data <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 2, 3, 4))

  out <- transform(data, cumx = cumsum(x), by = "g")
  expect_equal(out$cumx, c(1, 3, 3, 7))

  dev <- transform(data, dev = x - mean(x), by = "g")
  expect_equal(dev$dev, c(-0.5, 0.5, -0.5, 0.5))
})

test_that("transform(by =) resolves external variables from its caller", {
  data <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 2, 3, 4))

  run <- function(x) {
    offset <- 10
    transform(x, y = x + offset, by = "g")
  }

  expect_equal(run(data)$y, data$x + 10)
})

test_that("transform(by =) does not mutate its input", {
  input <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 3))
  original <- input

  transform(input, cumx = cumsum(x), by = "g")

  expect_identical(input, original)
})

# ---- within ----------------------------------------------------------------

test_that("within adds new columns", {
  out <- within(iris, ratio <- Sepal.Length / Sepal.Width)

  expect_s3_class(out, "basetable")
  expect_true("ratio" %in% names(out))
})

# ---- expr kernel -----------------------------------------------------------

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

# ---- pick ------------------------------------------------------------------

test_that("pick returns only selected columns", {
  out <- pick(mtcars, c("mpg", "cyl"))

  expect_s3_class(out, "basetable")
  expect_equal(names(out), c("mpg", "cyl"))
})

# ---- drop ------------------------------------------------------------------

test_that("drop removes selected columns", {
  out <- drop(mtcars, c("mpg", "cyl"))

  expect_s3_class(out, "basetable")
  expect_false(any(c("mpg", "cyl") %in% names(out)))
})

# ---- rename ----------------------------------------------------------------

test_that("renamecols changes column names", {
  out <- renamecols(mtcars, miles = mpg, cylinders = cyl)

  expect_s3_class(out, "basetable")
  expect_true(all(c("miles", "cylinders") %in% names(out)))
})

test_that("renamecols does not mutate its input", {
  input <- data.frame(id = c(1L, 1L, 2L), value = c(3, 3, 1))
  original <- input

  renamecols(input, key = id)

  expect_identical(input, original)
})
