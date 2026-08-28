test_that("btread infers types and matches read.csv", {
  for (d in list(iris, mtcars, ChickWeight)) {
    p <- tempfile(fileext = ".csv")
    write.csv(d, p, row.names = FALSE)
    a <- btread(p)
    b <- read.csv(p, stringsAsFactors = FALSE)
    expect_equal(a, b, ignore_attr = TRUE)
  }
})

test_that("btread handles column types", {
  p <- tempfile(fileext = ".csv")
  df <- data.frame(i = 1:3L, x = c(1.5, 2, NA), b = c(TRUE, FALSE, NA),
                   s = c("a", "b", NA), stringsAsFactors = FALSE)
  write.csv(df, p, row.names = FALSE)
  r <- btread(p)
  expect_type(r$i, "integer")
  expect_type(r$x, "double")
  expect_type(r$b, "logical")
  expect_type(r$s, "character")
  expect_identical(r$x, df$x)
  expect_identical(r$b, df$b)
})

test_that("btread quoting: embedded delimiter, quote, newline", {
  p <- tempfile(fileext = ".csv")
  writeLines(c('a,b', '"x,y",1', '"he said ""hi""",2', '"multi', 'line",3'), p)
  r <- btread(p)
  expect_equal(nrow(r), 3L)
  expect_identical(r$a, c("x,y", 'he said "hi"', "multi\nline"))
  expect_identical(r$b, 1:3L)
})

test_that("btread: no header, delimiter sniffing, skip, comment, n_max", {
  p <- tempfile(fileext = ".txt")
  writeLines(c("# note", "1;2;3", "4;5;6", "7;8;9"), p)
  r <- btread(p, header = FALSE, comment = "#")
  expect_identical(names(r), c("V1", "V2", "V3"))
  expect_identical(r$V1, c(1L, 4L, 7L))

  r2 <- btread(p, header = FALSE, comment = "#", n_max = 2)
  expect_equal(nrow(r2), 2L)

  p2 <- tempfile(fileext = ".csv")
  writeLines(c("skipme", "a,b", "1,2"), p2)
  expect_identical(btread(p2, skip = 1)$a, 1L)
})

test_that("btread col_select keeps only requested columns", {
  p <- tempfile(fileext = ".csv")
  write.csv(iris, p, row.names = FALSE)
  expect_named(btread(p, col_select = c("Sepal.Length", "Species")),
               c("Sepal.Length", "Species"))
  expect_named(btread(p, col_select = c(1L, 5L)),
               c("Sepal.Length", "Species"))
})

test_that("btread col_types override", {
  p <- tempfile(fileext = ".csv")
  writeLines(c("a,b,c", "1,2,3", "4,5,6"), p)
  r <- btread(p, col_types = c("double", "character", "skip"))
  expect_type(r$a, "double")
  expect_type(r$b, "character")
  expect_named(r, c("a", "b"))
})

test_that("btread lazy mode returns correct values", {
  p <- tempfile(fileext = ".csv")
  df <- data.frame(a = 1:500L, b = rnorm(500), c = 500:1L)
  write.csv(df, p, row.names = FALSE)
  r <- btread(p, lazy = TRUE)
  expect_equal(sum(r$a), sum(df$a))
  expect_equal(r$b, df$b)
  expect_equal(r$c, df$c)
  expect_s3_class(r, "data.frame")
})

test_that("btread lazy mode returns ALTREP character columns that behave", {
  n <- 5000L
  df <- data.frame(
    id = seq_len(n),
    s1 = sample(c("alpha", "beta,x", NA, "gamma"), n, replace = TRUE),
    v  = rnorm(n),
    s2 = sprintf("k%04d", sample(500L, n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  p <- tempfile(fileext = ".csv")
  data.table::fwrite(df, p)

  eager <- btread(p)
  lazy  <- btread(p, lazy = TRUE)

  expect_type(lazy$s1, "character")
  expect_identical(lazy$s1, eager$s1)
  expect_identical(lazy$s2, eager$s2)
  expect_identical(paste0(lazy$s2, "!")[1:5], paste0(eager$s2, "!")[1:5])
  expect_equal(sum(nchar(lazy$s2)), sum(nchar(eager$s2)))

  # element assignment forces + writes the cache
  lazy$s1[2] <- "ZZ"
  expect_identical(lazy$s1[2], "ZZ")

  # serialises without the mapping
  f <- tempfile()
  saveRDS(btread(p, lazy = TRUE), f)
  expect_equal(as.data.frame(readRDS(f)), as.data.frame(eager), ignore_attr = TRUE)

  # col_select through the lazy path keeps only requested columns unparsed
  sel <- btread(p, lazy = TRUE, col_select = c("id", "s2"))
  expect_named(sel, c("id", "s2"))
  expect_identical(sel$s2, eager$s2)
})

test_that("btread: NA strings and all-NA column", {
  p <- tempfile(fileext = ".csv")
  writeLines(c("a,b,c", "1,NA,", "2,x,", "3,NA,"), p)
  r <- btread(p, na = c("NA", ""))
  expect_true(is.na(r$b[1]))
  expect_true(all(is.na(r$c)))
})

test_that("btwrite round-trips through btread", {
  p <- tempfile(fileext = ".csv")
  df <- data.frame(
    i = c(1L, NA, 3L),
    x = c(1.25, NA, 3.5),
    b = c(TRUE, FALSE, NA),
    s = c("plain", "with,comma", 'with"quote'),
    f = factor(c("lo", "hi", "lo")),
    stringsAsFactors = FALSE
  )
  btwrite(df, p)
  r <- btread(p)
  expect_identical(r$i, df$i)
  expect_identical(r$x, df$x)
  expect_identical(r$b, df$b)
  expect_identical(r$s, df$s)
  expect_identical(r$f, as.character(df$f))
})

test_that("btwrite append does not repeat the header", {
  p <- tempfile(fileext = ".csv")
  btwrite(head(iris, 2), p)
  btwrite(iris[3:4, ], p, append = TRUE)
  expect_equal(nrow(btread(p)), 4L)
})

test_that("btread parallel indexer matches serial on a large file", {
  n <- 2e5L
  df <- data.frame(a = seq_len(n), b = runif(n), g = rep_len(letters, n))
  p <- tempfile(fileext = ".csv")
  data.table::fwrite(df, p)
  expect_gt(file.size(p), 2^20)               # large enough for the MT path

  r1 <- btread(p, n_threads = 1)
  r8 <- btread(p, n_threads = 8)
  expect_identical(r1, r8)
  expect_equal(r8$a, df$a)
  expect_equal(r8$b, df$b)
  expect_identical(r8$g, df$g)

  # n_max + header through the parallel path
  r <- btread(p, n_threads = 8, n_max = 1000)
  expect_equal(nrow(r), 1000L)
  expect_equal(r$a, df$a[1:1000])

  # no trailing newline
  raw <- readBin(p, "raw", file.size(p))
  if (raw[length(raw)] == as.raw(0x0a)) raw <- raw[-length(raw)]
  p2 <- tempfile(fileext = ".csv"); writeBin(raw, p2)
  r2 <- btread(p2, n_threads = 8)
  expect_equal(nrow(r2), n)
  expect_equal(r2$a[n], df$a[n])
})

test_that("btread reads gzip input", {
  p <- tempfile(fileext = ".csv")
  write.csv(head(mtcars), p, row.names = FALSE)
  pg <- paste0(p, ".gz")
  con <- gzfile(pg, "wb"); writeLines(readLines(p), con); close(con)
  r <- btread(pg)
  expect_equal(nrow(r), 6L)
  expect_equal(r$mpg, head(mtcars$mpg), ignore_attr = TRUE)
})
