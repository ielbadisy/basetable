library(basetable)
library(data.table)
library(bench)

set.seed(1)

n <- 1e5
grp_n <- 20

dt <- data.table(
  id = seq_len(n),
  grp = sample(letters[1:5], n, replace = TRUE),
  grp2 = sample(1:grp_n, n, replace = TRUE),
  value = rnorm(n),
  value2 = rnorm(n),
  txt = sample(c("aa", "bb", "cc", NA), n, replace = TRUE)
)

merge_n <- 5e4
x_dt <- data.table(id = seq_len(merge_n), t = sort(runif(merge_n)))
y_dt <- data.table(id = sample(seq_len(merge_n), merge_n), t = sort(runif(merge_n)))

na_dt <- data.table(
  grp = sample(letters[1:5], n, replace = TRUE),
  value = {
    v <- rnorm(n)
    v[sample(n, n * 0.3)] <- NA
    v
  }
)

workloads <- list(
  count = function() basetable::count(dt, by = "grp"),
  count_dt = function() dt[, .N, by = grp],

  duplicated_keys = function() basetable::duplicated_keys(dt, by = "grp2"),
  duplicated_keys_dt = function() dt[, .N, by = grp2][N > 1L],

  freq = function() basetable::freq(dt, column = "grp"),
  freq_dt = function() dt[, .N, by = grp],

  split = function() basetable::split(dt, by = "grp"),
  split_dt = function() split(dt, by = "grp", keep.by = FALSE),

  summarise = function() basetable::summarise(dt, m = mean(value), by = "grp"),
  summarise_dt = function() dt[, .(m = mean(value)), keyby = grp],

  filldown = function() basetable::filldown(na_dt, cols = "value", by = "grp"),
  filldown_dt = function() {
    out <- data.table::copy(na_dt)
    out[, value := data.table::nafill(value, type = "locf"), by = grp]
  },

  transform = function() basetable::transform(dt, z = value * 2, w = z + value2),
  transform_dt = function() {
    out <- data.table::copy(dt)
    out[, `:=`(z = value * 2)][, w := z + value2]
  },

  rollingmerge = function() basetable::rollingmerge(x_dt, y_dt, by = c("id", "t"), direction = "backward"),
  rollingmerge_dt = function() y_dt[x_dt, on = c("id", "t"), roll = TRUE],

  unite = function() basetable::unite(dt, column = "combo", cols = c("grp", "txt")),
  unite_dt = function() dt[, combo := paste(grp, txt, sep = "_")]
)

results <- lapply(names(workloads), function(nm) {
  t <- tryCatch(
    bench::mark(workloads[[nm]](), iterations = 15, check = FALSE, memory = FALSE)$median,
    error = function(e) NA
  )
  data.table(name = nm, median_ms = as.numeric(t) * 1000)
})

results <- data.table::rbindlist(results)
results[, workload := sub("_dt$", "", name)]
results[, kind := ifelse(grepl("_dt$", name), "data_table", "basetable")]
wide <- data.table::dcast(results, workload ~ kind, value.var = "median_ms")
wide[, overhead_ratio := basetable / data_table]
data.table::setorder(wide, -overhead_ratio)

print(wide)
