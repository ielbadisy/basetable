split <- function(data, by, drop = FALSE, keep.by = FALSE) {
  dt <- bt_as_data_table_ro(data)
  by <- bt_resolve_cols(dt, by)
  out <- base::split(dt, by = by, drop = drop, keep.by = keep.by, sorted = TRUE)
  lapply(out, bt_as_tibble)
}

by_apply <- function(data, by, fun, ..., bind = FALSE, id = ".group") {
  pieces <- split(data, by = by, keep.by = TRUE)
  out <- functionals::fmap(pieces, function(piece) fun(piece, ...))

  if (!bind) {
    return(out)
  }

  combine(out, id = id)
}

combine <- function(x, id = NULL) {
  if (!is.list(x)) {
    stop("`x` must be a list.", call. = FALSE)
  }

  out <- data.table::rbindlist(x, fill = TRUE, idcol = id)
  bt_as_tibble(out)
}
