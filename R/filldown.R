filldown <- function(data, cols, by = NULL) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  by <- if (is.null(by)) character(0) else bt_resolve_cols(df, by)

  if (length(cols) < 1L) {
    stop("`cols` must contain at least one column.", call. = FALSE)
  }

  fill_forward <- function(x) {
    last <- NULL
    has_last <- FALSE

    for (i in seq_along(x)) {
      if (is.na(x[[i]])) {
        if (has_last) {
          x[[i]] <- last
        }
      } else {
        last <- x[[i]]
        has_last <- TRUE
      }
    }

    x
  }

  if (length(by) == 0L) {
    for (nm in cols) {
      df[[nm]] <- fill_forward(df[[nm]])
    }
  } else {
    groups <- base::split(seq_len(nrow(df)), interaction(df[, by, drop = FALSE], drop = TRUE, lex.order = TRUE))
    for (rows in groups) {
      for (nm in cols) {
        df[[nm]][rows] <- fill_forward(df[[nm]][rows])
      }
    }
  }

  bt_as_tibble(df)
}
