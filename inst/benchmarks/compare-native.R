# Compare an installed candidate with a separately built baseline in one R
# process. Both libraries see the same input objects and operation expressions.
# BT_BASELINE_DLL=/path/to/basetable.so BT_OPERATIONS=filter Rscript inst/benchmarks/compare-native.R
for (expr in parse("inst/benchmarks/make-readme-figures.R")) {
  if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("nthreads_mt"))) break
  eval(expr, .GlobalEnv)
}
baseline_path <- Sys.getenv("BT_BASELINE_DLL")
stopifnot(nzchar(baseline_path), file.exists(baseline_path))
baseline_dll <- dyn.load(baseline_path)
baseline_symbols <- getDLLRegisteredRoutines(baseline_dll)$.Call
ns <- asNamespace("basetable")
symbol_names <- intersect(names(baseline_symbols), ls(ns, all.names = TRUE))
candidate_symbols <- mget(symbol_names, ns)
stopifnot(!identical(candidate_symbols$bt_filter_$address,
                    baseline_symbols$bt_filter_$address))
use_symbols <- function(symbols) {
  for (name in symbol_names) {
    unlockBinding(name, ns)
    assign(name, symbols[[name]], ns)
    lockBinding(name, ns)
  }
}
bench_one <- function(label, exprs) {
  selected <- Sys.getenv("BT_OPERATIONS", "")
  if (nzchar(selected) && !label %in% strsplit(selected, ",", fixed = TRUE)[[1L]])
    return(NULL)
  on.exit(use_symbols(candidate_symbols), add = TRUE)
  use_symbols(candidate_symbols)
  candidate <- eval(exprs$basetable, .GlobalEnv)
  use_symbols(baseline_symbols)
  baseline <- eval(exprs$basetable, .GlobalEnv)
  stopifnot(isTRUE(all.equal(candidate, baseline)))
  rows <- list()
  for (batch in 1:4) {
    engines <- c("candidate", "baseline", "collapse")
    if (batch %% 2 == 0) engines <- rev(engines)
    for (engine in engines) {
      use_symbols(if (engine == "baseline") baseline_symbols else candidate_symbols)
      e <- exprs[[if (engine == "collapse") "collapse" else "basetable"]]
      invisible(eval(e, .GlobalEnv))
      gc()
      m <- bench::mark(eval(e, .GlobalEnv), iterations = REPS,
                       check = FALSE, memory = TRUE, filter_gc = FALSE)
      rows[[length(rows) + 1L]] <- data.frame(operation = label, engine, batch,
        median_ms = as.numeric(m$median) * 1000)
    }
  }
  out <- do.call(rbind, rows)
  print(out, row.names = FALSE)
  # measure_all applies the published engine factor levels; retain A/B labels
  # separately so they are not converted to missing values by that formatting.
  out$variant <- out$engine
  out
}
results <- list()
for (threads in unique(c(1L, parallel::detectCores()))) {
  set_threads(threads = threads)
  result <- measure_all()
  result$engine <- result$variant
  result$variant <- NULL
  result$threads <- threads
  results[[length(results) + 1L]] <- result
}
use_symbols(candidate_symbols)
result <- do.call(rbind, results)
output <- Sys.getenv("BT_RESULTS")
if (nzchar(output)) write.csv(result, output, row.names = FALSE)
print(stats::aggregate(median_ms ~ operation + engine + threads, result, median), row.names = FALSE)
