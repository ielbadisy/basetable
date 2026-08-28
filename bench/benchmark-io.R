# Benchmark basetable::btread / btwrite against data.table::fread and vroom.
#
#   Rscript bench/benchmark-io.R
#
# Scenarios: wide-numeric, long-numeric, mixed, string-heavy, subset-read,
# write. For each we time several basetable configurations plus the
# competitors and report the median.

suppressMessages({
  library(basetable)
  library(data.table)
  library(vroom)
  library(bench)
})

set.seed(1)
tmp <- tempfile("btbench_"); dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE))
DT_THREADS <- max(1L, getDTthreads())
BT_THREADS <- min(8L, DT_THREADS)
cat(sprintf("threads: data.table=%d  basetable=%d\n\n", DT_THREADS, BT_THREADS))

gen <- function(name, df) {
  path <- file.path(tmp, name)
  fwrite(df, path)
  cat(sprintf("  %-22s %6.1f MB  (%d x %d)\n", name,
              file.size(path) / 1e6, nrow(df), ncol(df)))
  path
}

cat("generating fixtures\n")
n_long <- 2e6L
long_num <- gen("long_numeric.csv", data.frame(
  a = sample.int(1e6L, n_long, TRUE),
  b = runif(n_long),
  c = rnorm(n_long),
  d = sample.int(100L, n_long, TRUE)
))

n_wide <- 60000L; p_wide <- 50L
wide_num <- gen("wide_numeric.csv", as.data.frame(
  matrix(rnorm(n_wide * p_wide), n_wide, p_wide,
         dimnames = list(NULL, paste0("x", seq_len(p_wide))))
))

n_mix <- 1e6L
mixed <- gen("mixed.csv", data.frame(
  id    = seq_len(n_mix),
  grp   = sample(letters, n_mix, TRUE),
  val   = rnorm(n_mix),
  flag  = sample(c(TRUE, FALSE), n_mix, TRUE),
  count = sample.int(500L, n_mix, TRUE),
  name  = sprintf("item_%06d", sample.int(1e5L, n_mix, TRUE))
))

n_str <- 5e5L
strheavy <- gen("string_heavy.csv", as.data.frame(
  matrix(sample(stringi::stri_rand_strings(2000, 12), n_str * 10L, TRUE),
         n_str, 10L, dimnames = list(NULL, paste0("s", 1:10)))
))

run <- function(label, exprs, iterations = 5) {
  cat(sprintf("\n== %s ==\n", label))
  res <- bench::mark(exprs = exprs, check = FALSE, min_iterations = iterations,
                     filter_gc = FALSE)
  res <- res[order(res$median), c("expression", "median", "mem_alloc")]
  res$median <- as.numeric(res$median)
  best <- res$expression[1]
  for (i in seq_len(nrow(res)))
    cat(sprintf("  %-34s %8.3f s   %8s   %s\n",
                res$expression[i], res$median[i],
                format(res$mem_alloc[i]),
                if (i == 1) "<- fastest" else
                  sprintf("%.2fx", res$median[i] / res$median[1])))
  invisible(res)
}

# ---- long numeric ---------------------------------------------------------
run("long numeric  (2e6 x 4, full read)", list(
  `bt eager 1t`      = quote(btread(long_num, n_threads = 1)),
  `bt eager Nt`      = quote(btread(long_num, n_threads = BT_THREADS)),
  `bt lazy (all)`    = quote({d <- btread(long_num, lazy = TRUE); lapply(d, function(x) x[1L]); d}),
  `fread 1t`         = quote(fread(long_num, nThread = 1)),
  `fread Nt`         = quote(fread(long_num, nThread = DT_THREADS)),
  `vroom`            = quote(vroom(long_num, progress = FALSE, show_col_types = FALSE)),
  `vroom altrep`     = quote(vroom(long_num, progress = FALSE, show_col_types = FALSE, altrep = TRUE)),
  `read.csv`         = quote(read.csv(long_num))
))

# ---- wide numeric -------------------------------------------------------
run("wide numeric  (6e4 x 50, full read)", list(
  `bt eager 1t`   = quote(btread(wide_num, n_threads = 1)),
  `bt eager Nt`   = quote(btread(wide_num, n_threads = BT_THREADS)),
  `fread 1t`      = quote(fread(wide_num, nThread = 1)),
  `fread Nt`      = quote(fread(wide_num, nThread = DT_THREADS)),
  `vroom`         = quote(vroom(wide_num, progress = FALSE, show_col_types = FALSE))
))

# ---- wide numeric, subset of columns ----------------------------------
run("wide numeric  (6e4 x 50, read 3 cols)", list(
  `bt lazy + select`  = quote(btread(wide_num, lazy = TRUE, col_select = c(1L, 25L, 50L))),
  `bt eager + select` = quote(btread(wide_num, col_select = c(1L, 25L, 50L))),
  `fread select`      = quote(fread(wide_num, select = c(1L, 25L, 50L), nThread = DT_THREADS)),
  `vroom select`      = quote(vroom(wide_num, col_select = c(1, 25, 50), progress = FALSE, show_col_types = FALSE)),
  `vroom altrep sel`  = quote(vroom(wide_num, col_select = c(1, 25, 50), altrep = TRUE, progress = FALSE, show_col_types = FALSE))
))

# ---- mixed ------------------------------------------------------------
run("mixed types  (1e6 x 6, full read)", list(
  `bt eager Nt`  = quote(btread(mixed, n_threads = BT_THREADS)),
  `bt lazy`      = quote({d <- btread(mixed, lazy = TRUE); lapply(d, function(x) x[1L]); d}),
  `fread Nt`     = quote(fread(mixed, nThread = DT_THREADS)),
  `vroom`        = quote(vroom(mixed, progress = FALSE, show_col_types = FALSE))
))

# ---- string heavy ---------------------------------------------------
run("string heavy  (5e5 x 10, full read)", list(
  `bt eager Nt`  = quote(btread(strheavy, n_threads = BT_THREADS)),
  `fread Nt`     = quote(fread(strheavy, nThread = DT_THREADS)),
  `vroom`        = quote(vroom(strheavy, progress = FALSE, show_col_types = FALSE)),
  `vroom altrep` = quote(vroom(strheavy, altrep = TRUE, progress = FALSE, show_col_types = FALSE))
))

# ---- write --------------------------------------------------------
dmix <- as.data.frame(fread(mixed))
o1 <- file.path(tmp, "o_bt.csv"); o2 <- file.path(tmp, "o_fw.csv"); o3 <- file.path(tmp, "o_vr.csv")
run("write  (1e6 x 6)", list(
  `btwrite Nt`  = quote(btwrite(dmix, o1, n_threads = BT_THREADS)),
  `btwrite 1t`  = quote(btwrite(dmix, o1, n_threads = 1)),
  `fwrite Nt`    = quote(fwrite(dmix, o2, nThread = DT_THREADS)),
  `vroom_write`  = quote(vroom_write(dmix, o3, delim = ","))
))

cat("\ndone\n")
