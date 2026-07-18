crossmerge <- function(x, y) {
  x_dt <- bt_as_data_table(x)
  y_dt <- bt_as_data_table(y)
  x_dt[[".bt_cross_key"]] <- 1L
  y_dt[[".bt_cross_key"]] <- 1L
  out <- data.table::merge.data.table(x_dt, y_dt, by = ".bt_cross_key", sort = FALSE, allow.cartesian = TRUE)
  out[[".bt_cross_key"]] <- NULL
  bt_as_tibble(out)
}
