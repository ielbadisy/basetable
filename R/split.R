split <- function(data, by, drop = FALSE, keep.by = FALSE) {
  df <- bt_as_data_frame(data)
  by <- bt_resolve_cols(df, by)
  key <- interaction(df[, by, drop = FALSE], drop = drop, lex.order = TRUE)
  out <- base::split(df, key, drop = drop)

  if (!keep.by) {
    out <- lapply(out, function(piece) bt_as_tibble(piece[, setdiff(names(piece), by), drop = FALSE]))
  } else {
    out <- lapply(out, bt_as_tibble)
  }

  out
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
