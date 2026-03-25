library(basetable)
library(data.table)

set.seed(1)
n <- 1e5
x <- data.frame(
  id = seq_len(n),
  grp = sample(letters[1:5], n, replace = TRUE),
  value = rnorm(n)
)

bench_subset <- function() {
  system.time(subset(x, value > 0, select = c("id", "grp", "value")))
  system.time(base::subset(x, value > 0, select = c("id", "grp", "value")))
}

print(bench_subset())
