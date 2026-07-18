dims <- function(data) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(data.frame(rows = nrow(df), cols = ncol(df), stringsAsFactors = FALSE))
}

types <- function(data) {
  df <- bt_as_data_frame(data)
  bt_as_tibble(data.frame(
    column = names(df),
    class = vapply(df, bt_mode, character(1)),
    typeof = vapply(df, typeof, character(1)),
    stringsAsFactors = FALSE
  ))
}

headtail <- function(data, n = 3) {
  df <- bt_as_data_frame(data)
  if (nrow(df) <= (2L * n)) {
    return(bt_as_tibble(df))
  }
  idx <- c(seq_len(n), seq.int(nrow(df) - n + 1L, nrow(df)))
  bt_as_tibble(df[idx, , drop = FALSE])
}

glimpse <- function(data, width = getOption("width")) {
  df <- bt_as_data_frame(data)
  cat(sprintf("Rows: %s  Columns: %s\n", nrow(df), ncol(df)))
  for (nm in names(df)) {
    vals <- paste(utils::head(as.character(df[[nm]]), 3L), collapse = ", ")
    line <- sprintf("$ %s <%s> %s", nm, bt_mode(df[[nm]]), vals)
    cat(substr(line, 1L, width), "\n", sep = "")
  }
  invisible(data)
}

describe <- function(data, cols = NULL, top_n = 3) {
  df <- bt_as_data_frame(data)
  if (!is.null(cols)) {
    cols <- bt_resolve_cols(df, cols)
    df <- df[, cols, drop = FALSE]
  }

  rows <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    is_num <- is.numeric(x)
    qs <- if (is_num) stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE) else rep(NA_real_, 3L)

    data.frame(
      column = nm,
      class = bt_mode(x),
      n = length(x),
      missing = sum(is.na(x)),
      missing_prop = mean(is.na(x)),
      distinct = bt_distinct_n(x),
      mean = if (is_num) mean(x, na.rm = TRUE) else NA_real_,
      sd = if (is_num) stats::sd(x, na.rm = TRUE) else NA_real_,
      min = if (is_num) suppressWarnings(min(x, na.rm = TRUE)) else NA_real_,
      q25 = qs[[1L]],
      median = qs[[2L]],
      q75 = qs[[3L]],
      max = if (is_num) suppressWarnings(max(x, na.rm = TRUE)) else NA_real_,
      top = if (!is_num) bt_top_values(x, n = top_n) else "",
      stringsAsFactors = FALSE
    )
  })

  bt_as_tibble(do.call(rbind, rows))
}

profile <- function(data, cols = NULL, top_n = 3) {
  describe(data, cols = cols, top_n = top_n)
}

freq <- function(data, column, by = NULL, prop = FALSE, sort = TRUE) {
  dt <- bt_as_data_table_ro(data)
  column <- bt_resolve_cols(dt, column)
  if (length(column) != 1L) {
    stop("`column` must name exactly one column.", call. = FALSE)
  }

  by <- if (is.null(by)) character(0) else bt_resolve_cols(dt, by)
  groups <- c(by, column)
  out <- dt[, list(n = .N), by = groups]

  if (prop) {
    if (length(by) == 0L) {
      out[, prop := n / sum(n)]
    } else {
      out[, prop := n / sum(n), by = by]
    }
  }

  if (sort) {
    data.table::setorderv(out, "n", order = -1L)
  }

  bt_as_tibble(out)
}

#' Table 1-style descriptive summary
#'
#' Build a publication-style summary table of one or more variables, optionally
#' stratified by a grouping column, in the manner of a clinical "Table 1".
#'
#' @param data A data.frame or data.table.
#' @param vars Character vector of variables to summarize. Defaults to every
#'   column other than `by`.
#' @param by Optional single column name used to stratify the summary.
#' @param overall Include an "Overall" column alongside the by-group columns.
#' @param p_value Include a column of between-group p-values. Requires `by`.
#' @param digits Number of decimal places used when formatting numbers.
#'
#' @return A tibble with one row per variable (or variable level), and one
#'   column per stratum plus `Overall`/`p_value` as requested.
#' @export
summarytab <- function(data, vars = NULL, by = NULL, overall = TRUE, p_value = FALSE, digits = 1) {
  df <- bt_as_data_frame(data)

  if (!is.null(by)) {
    by <- bt_resolve_cols(df, by)
    if (length(by) != 1L) {
      stop("`by` must name exactly one column.", call. = FALSE)
    }
  }

  if (is.null(vars)) {
    vars <- setdiff(names(df), by)
  } else {
    vars <- bt_resolve_cols(df, vars)
  }

  if (!is.null(by) && any(vars %in% by)) {
    stop("`vars` must not include the stratification column.", call. = FALSE)
  }
  if (length(vars) == 0L) {
    stop("`vars` must contain at least one column.", call. = FALSE)
  }
  if (p_value && is.null(by)) {
    stop("`p_value = TRUE` requires `by`.", call. = FALSE)
  }
  if (is.null(by) && !overall) {
    stop("`overall` must be TRUE when `by` is NULL.", call. = FALSE)
  }

  group <- if (is.null(by)) NULL else df[[by]]
  group_keys <- if (is.null(group)) character(0) else bt_summarytab_keys(group)
  group_labels <- vapply(group_keys, bt_summarytab_label, character(1))

  rows <- vector("list", length = 0L)

  for (var in vars) {
    x <- df[[var]]
    p_value_label <- if (p_value) bt_summarytab_p_value(x, group) else NULL

    if (is.numeric(x)) {
      group_values <- if (length(group_keys) > 0L) {
        vapply(group_keys, function(key) {
          bt_summarytab_numeric(x[bt_summarytab_match(group, key)], digits = digits)
        }, character(1))
      } else {
        character(0)
      }

      rows[[length(rows) + 1L]] <- bt_summarytab_row(
        variable = var,
        level = "Mean (SD)",
        group_labels = group_labels,
        group_values = group_values,
        overall = overall,
        overall_value = bt_summarytab_numeric(x, digits = digits),
        p_value = p_value,
        p_value_label = p_value_label
      )

      if (any(is.na(x))) {
        missing_values <- if (length(group_keys) > 0L) {
          vapply(group_keys, function(key) {
            mask <- bt_summarytab_match(group, key)
            bt_summarytab_count_pct(sum(is.na(x[mask])), sum(mask), digits = digits)
          }, character(1))
        } else {
          character(0)
        }

        rows[[length(rows) + 1L]] <- bt_summarytab_row(
          variable = "",
          level = "Missing",
          group_labels = group_labels,
          group_values = missing_values,
          overall = overall,
          overall_value = bt_summarytab_count_pct(sum(is.na(x)), length(x), digits = digits),
          p_value = p_value,
          p_value_label = ""
        )
      }

      next
    }

    level_keys <- bt_summarytab_keys(x)
    for (i in seq_along(level_keys)) {
      level_key <- level_keys[[i]]
      level_mask <- bt_summarytab_match(x, level_key)
      group_values <- if (length(group_keys) > 0L) {
        vapply(group_keys, function(key) {
          mask <- bt_summarytab_match(group, key)
          bt_summarytab_count_pct(sum(level_mask & mask, na.rm = TRUE), sum(mask), digits = digits)
        }, character(1))
      } else {
        character(0)
      }

      rows[[length(rows) + 1L]] <- bt_summarytab_row(
        variable = if (i == 1L) var else "",
        level = bt_summarytab_label(level_key),
        group_labels = group_labels,
        group_values = group_values,
        overall = overall,
        overall_value = bt_summarytab_count_pct(sum(level_mask, na.rm = TRUE), length(x), digits = digits),
        p_value = p_value,
        p_value_label = if (i == 1L) p_value_label else ""
      )
    }
  }

  bt_as_tibble(do.call(rbind, rows))
}

compare <- function(x, y, by = NULL) {
  x_df <- bt_as_data_frame(x)
  y_df <- bt_as_data_frame(y)

  out <- list(
    dims = data.frame(
      object = c("x", "y"),
      rows = c(nrow(x_df), nrow(y_df)),
      cols = c(ncol(x_df), ncol(y_df)),
      stringsAsFactors = FALSE
    ),
    names = data.frame(
      column = union(names(x_df), names(y_df)),
      in_x = union(names(x_df), names(y_df)) %in% names(x_df),
      in_y = union(names(x_df), names(y_df)) %in% names(y_df),
      stringsAsFactors = FALSE
    ),
    types = merge(types(x_df), types(y_df), by = "column", all = TRUE, suffixes = c(".x", ".y")),
    missing = merge(
      missingness(x_df, margin = "column"),
      missingness(y_df, margin = "column"),
      by = "column",
      all = TRUE,
      suffixes = c(".x", ".y")
    )
  )

  if (!is.null(by)) {
    by <- bt_resolve_cols(x_df, by)
    bt_resolve_cols(y_df, by)
    x_keys <- unique(x_df[, by, drop = FALSE])
    y_keys <- unique(y_df[, by, drop = FALSE])
    out$key_overlap <- data.frame(
      x_unique = nrow(x_keys),
      y_unique = nrow(y_keys),
      common = nrow(merge(x_keys, y_keys, by = by)),
      stringsAsFactors = FALSE
    )
  }

  lapply(out, function(x) {
    if (inherits(x, "data.frame")) bt_as_tibble(x) else x
  })
}

bt_summarytab_row <- function(variable, level, group_labels, group_values, overall, overall_value, p_value, p_value_label) {
  out <- list(variable = variable, level = level)

  if (length(group_labels) > 0L) {
    for (i in seq_along(group_labels)) {
      out[[group_labels[[i]]]] <- group_values[[i]]
    }
  }

  if (overall) {
    out$Overall <- overall_value
  }
  if (p_value) {
    out$p_value <- p_value_label
  }

  bt_as_tibble(as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE))
}

bt_summarytab_numeric <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return("NA")
  }

  paste0(
    bt_summarytab_number(mean(x), digits = digits),
    " (",
    bt_summarytab_number(stats::sd(x), digits = digits),
    ")"
  )
}

bt_summarytab_count_pct <- function(n, total, digits = 1) {
  if (total == 0L) {
    return("NA")
  }

  paste0(
    n,
    " (",
    bt_summarytab_number(100 * (n / total), digits = digits),
    "%)"
  )
}

bt_summarytab_number <- function(x, digits = 1) {
  if (is.na(x)) {
    return("NA")
  }

  formatC(x, digits = digits, format = "f")
}

bt_summarytab_keys <- function(x) {
  x_chr <- as.character(x)
  observed <- unique(x_chr[!is.na(x_chr)])

  if (is.factor(x)) {
    observed <- levels(x)[levels(x) %in% observed]
  }

  if (any(is.na(x))) {
    c(observed, "<NA>")
  } else {
    observed
  }
}

bt_summarytab_label <- function(key) {
  if (identical(key, "<NA>")) "Missing" else key
}

bt_summarytab_match <- function(x, key) {
  if (identical(key, "<NA>")) {
    is.na(x)
  } else {
    !is.na(x) & as.character(x) == key
  }
}

bt_summarytab_p_value <- function(x, group) {
  keep <- !is.na(group)
  x <- x[keep]
  group <- group[keep]

  if (length(group) == 0L || length(unique(group)) < 2L) {
    return(NA_character_)
  }

  p <- tryCatch({
    if (is.numeric(x)) {
      keep_x <- !is.na(x)
      x <- x[keep_x]
      group <- droplevels(as.factor(group[keep_x]))

      if (length(x) == 0L || length(unique(group)) < 2L) {
        NA_real_
      } else if (nlevels(group) == 2L) {
        stats::t.test(x ~ group)$p.value
      } else {
        stats::oneway.test(x ~ group)$p.value
      }
    } else {
      x_tab <- ifelse(is.na(x), "Missing", as.character(x))
      tbl <- table(x_tab, group, useNA = "ifany")

      if (nrow(tbl) < 2L || ncol(tbl) < 2L) {
        NA_real_
      } else {
        suppressWarnings(stats::chisq.test(tbl)$p.value)
      }
    }
  }, error = function(e) {
    if (!is.numeric(x)) {
      tbl <- table(ifelse(is.na(x), "Missing", as.character(x)), group, useNA = "ifany")
      if (all(dim(tbl) == c(2L, 2L))) {
        return(stats::fisher.test(tbl)$p.value)
      }
    }
    NA_real_
  })

  if (is.na(p)) {
    return(NA_character_)
  }

  format.pval(p, digits = 3, eps = 0.001)
}
