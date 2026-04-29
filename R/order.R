reorder <- function(data, by, decreasing = FALSE, na.last = TRUE) {
  dt <- bt_as_data_table(data)
  by <- bt_resolve_cols(dt, by)
  decreasing <- bt_recycle_flag(decreasing, length(by), "decreasing")
  data.table::setorderv(dt, cols = by, order = ifelse(decreasing, -1L, 1L), na.last = na.last)
  bt_as_tibble(dt)
}
