# Reproducible end-to-end timings plus optional native phase diagnostics.
# BT_PROFILE=1 Rscript inst/benchmarks/profile-core.R
# BT_FIG_N=100000 BT_FIG_REPS=20 Rscript inst/benchmarks/profile-core.R
# Uses the exact README expressions; never changes published figures.
expressions <- parse("inst/benchmarks/make-readme-figures.R")
for (expr in expressions) {
  if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("nthreads_mt"))) break
  eval(expr, .GlobalEnv)
}
bench_one <- function(label, exprs) {
  exprs <- exprs[c("basetable", "collapse")]
  # Compare every column independently of container class and row names.
  a <- as.data.frame(eval(exprs[[1]], .GlobalEnv))
  b <- as.data.frame(eval(exprs[[2]], .GlobalEnv))
  if (label == "count by group") {
    a <- a[order(a$gh), ]; b <- b[order(b$gh), ]
    names(b) <- names(a)
  }
  if (label == "sd by group") {
    a <- a[order(a$g), ]; b <- b[order(b$g), ]
  }
  if (label == "equi join") {
    stopifnot(identical(names(b), c("g", "gh", "x", "y", "id", "y_dim_tbl")))
    names(b)[c(4L, 6L)] <- c("y.x", "y.y")
  }
  stopifnot(nrow(a) == nrow(b), identical(names(a), names(b)))
  for (nm in names(a)) stopifnot(isTRUE(all.equal(a[[nm]], b[[nm]], tolerance = 1e-10)))
  if (Sys.getenv("BT_PROFILE") == "1") {
    cat("OPERATION,", label, "\n", sep = "")
    for (i in 1:3) invisible(eval(exprs[[1]], .GlobalEnv))
    return(data.frame(operation = label, engine = names(exprs), median_ms = NA, mem_mb = NA))
  }
  # Warm both engines; repeated alternating batches reduce order bias.
  rows <- list()
  for (batch in 1:3) {
    for (engine in if (batch %% 2) names(exprs) else rev(names(exprs))) {
      e <- exprs[[engine]]
      gc()
      m <- bench::mark(eval(e, .GlobalEnv), iterations = REPS,
                       check = FALSE, memory = TRUE, filter_gc = FALSE)
      rows[[length(rows) + 1L]] <- data.frame(operation = label, engine,
        batch, median_ms = as.numeric(m$median) * 1000,
        mem_mb = as.numeric(m$mem_alloc) / 1024^2)
    }
  }
  ans <- do.call(rbind, rows)
  print(ans, row.names = FALSE)
  ans
}
results <- list()
for (threads in unique(c(1L, parallel::detectCores()))) {
  set_threads(threads = threads)
  cat("THREADS,", threads, "\n", sep = "")
  result <- measure_all()
  result$threads <- threads
  results[[length(results) + 1L]] <- result
}
path <- Sys.getenv("BT_RESULTS", "")
if (nzchar(path)) write.csv(do.call(rbind, results), path, row.names = FALSE)
print(sessionInfo())
