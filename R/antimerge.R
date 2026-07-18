antimerge <- function(x, y, by) {
  x_dt <- bt_as_data_frame(x)
  y_dt <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x_dt, by)
  bt_resolve_cols(y_dt, by)

  x_key <- interaction(x_dt[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  y_key <- interaction(y_dt[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)

  bt_as_tibble(x_dt[!(x_key %in% y_key), , drop = FALSE])
}
