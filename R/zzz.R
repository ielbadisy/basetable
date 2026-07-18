# Column names referenced via data.table's NSE (e.g. `dt[, .(N = .N)]`,
# `dt[, prop := n / sum(n)]`) look like undefined globals to static analysis
# tools; they are data.table in-scope symbols, not package-level variables.
utils::globalVariables(c("N", "n", "prop"))
