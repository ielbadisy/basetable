crossmerge <- function(x, y) {
  x_dt <- data.table::copy(bt_as_data_table_ro(x))
  y_dt <- data.table::copy(bt_as_data_table_ro(y))
  x_dt[[".bt_cross_key"]] <- 1L
  y_dt[[".bt_cross_key"]] <- 1L
  out <- data.table::merge.data.table(x_dt, y_dt, by = ".bt_cross_key", sort = FALSE, allow.cartesian = TRUE)
  out[[".bt_cross_key"]] <- NULL
  out
}
