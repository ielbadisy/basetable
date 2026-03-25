library(basetable)
library(data.table)

set.seed(1)
n <- 1e5
x <- data.frame(
  grp = sample(letters[1:5], n, replace = TRUE),
  value = rnorm(n)
)

bench_aggregate <- function() {
  system.time(aggregate(x, by = "grp", value = "value", fun = mean))
  dt <- as.data.table(x)
  system.time(dt[, .(value = mean(value)), by = grp])
}

print(bench_aggregate())
