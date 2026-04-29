reshape <- function(data, ..., direction) {
  bt_as_tibble(stats::reshape(bt_as_data_frame(data), ..., direction = direction))
}

stack <- function(data, select = NULL, drop = FALSE) {
  df <- bt_as_data_frame(data)
  if (is.null(select)) {
    select <- names(df)
  }
  select <- bt_resolve_cols(df, select)
  bt_as_tibble(utils::stack(df[, select, drop = drop]))
}

unstack <- function(data, form, ...) {
  bt_as_tibble(utils::unstack(bt_as_data_frame(data), form = form, ...))
}
