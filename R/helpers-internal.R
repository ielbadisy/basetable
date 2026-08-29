# Wrap a frame as the basetable result class (see R/bt-table.R). The name is
# kept for its many call sites; the output no longer has any data.table class.
bt_as_data_table <- function(data) {
  if (!is.data.frame(data) && !is.list(data)) {
    stop("`data` must be a data.frame or list of equal-length columns.", call. = FALSE)
  }
  new_basetable(data)
}

bt_data_mask <- function(data, parent = parent.frame()) {
  list2env(as.list(data), parent = parent)
}

# Like bt_as_data_table(), but skips the defensive copy when `data` is already
# a compatible table. Only safe for callers that never mutate the result in
# place.
bt_as_data_table_ro <- function(data) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  bt_as_data_frame(data)
}

bt_as_data_frame <- function(data) {
  if (is.data.frame(data)) {
    if (!identical(class(data), "data.frame")) {
      class(data) <- "data.frame"
    }
    return(data)
  }
  as.data.frame(data, stringsAsFactors = FALSE)
}

bt_group_rows <- function(group_id) {
  n <- if (length(group_id) == 0L) 0L else max(group_id)
  rows <- vector("list", n)
  for (g in seq_len(n)) {
    rows[[g]] <- which(group_id == g)
  }
  rows
}

bt_rbind_fill <- function(dfs, fill = TRUE, id = NULL) {
  if (length(dfs) == 0L) {
    return(bt_as_data_table(data.frame()))
  }
  id_values <- names(dfs)
  dfs <- lapply(unname(dfs), bt_as_data_frame)
  bt_as_data_table(.Call(
    bt_rbind_,
    dfs,
    isTRUE(fill),
    if (is.null(id)) NULL else as.character(id)[[1L]],
    if (is.null(id_values)) NULL else as.character(id_values)
  ))
}

# Comparison-operator codes shared with the native range-join kernel.
bt_cmp_ops <- c("<" = 0L, "<=" = 1L, ">" = 2L, ">=" = 3L, "==" = 4L)

bt_first_match <- function(x, y, by) {
  x <- bt_as_data_frame(x)
  y <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x, by)
  bt_resolve_cols(y, by)
  .Call(
    bt_first_match_, x, y,
    as.integer(match(by, names(x))),
    as.integer(match(by, names(y)))
  )
}

bt_join_rows <- function(x, y, by, all.x = FALSE, all.y = FALSE, suffixes = c(".x", ".y")) {
  x <- bt_as_data_frame(x)
  y <- bt_as_data_frame(y)
  by <- bt_resolve_cols(x, by)
  bt_resolve_cols(y, by)
  bt_as_data_table(.Call(
    bt_join_, x, y,
    as.integer(match(by, names(x))),
    as.integer(match(by, names(y))),
    isTRUE(all.x), isTRUE(all.y),
    as.character(suffixes)
  ))
}

# Equi keys (optional) plus (x col, op, y col) comparison predicates. Output is
# every x column followed by `y_cols`; unmatched x rows survive with NA y values
# when all.x is TRUE.
bt_range_join <- function(x, y, by = character(0), predicates = list(),
                          y_cols, all.x = FALSE) {
  x <- bt_as_data_frame(x)
  y <- bt_as_data_frame(y)
  by <- if (length(by) == 0L) character(0) else bt_resolve_cols(x, by)
  if (length(by) > 0L) bt_resolve_cols(y, by)
  px <- vapply(predicates, function(p) p$x, character(1))
  py <- vapply(predicates, function(p) p$y, character(1))
  ops <- vapply(predicates, function(p) unname(bt_cmp_ops[[p$op]]), integer(1))
  if (length(px) > 0L) {
    bt_resolve_cols(x, px)
    bt_resolve_cols(y, py)
  }
  y_cols <- bt_resolve_cols(y, y_cols)
  bt_as_data_table(.Call(
    bt_range_join_, x, y,
    as.integer(match(by, names(x))),
    as.integer(match(by, names(y))),
    as.integer(match(px, names(x))),
    as.integer(match(py, names(y))),
    as.integer(ops),
    as.integer(match(y_cols, names(y))),
    isTRUE(all.x)
  ))
}

# Exact keys (optional) plus one ordered roll key. direction: "backward" (y<=x),
# "forward" (y>=x), or "nearest". Every x row is kept.
bt_rolling_join <- function(x, y, exact = character(0), x_roll, y_roll,
                            direction = "backward", tolerance = Inf, y_cols) {
  x <- bt_as_data_frame(x)
  y <- bt_as_data_frame(y)
  exact <- if (length(exact) == 0L) character(0) else bt_resolve_cols(x, exact)
  if (length(exact) > 0L) bt_resolve_cols(y, exact)
  x_roll <- bt_resolve_cols(x, x_roll)
  y_roll <- bt_resolve_cols(y, y_roll)
  y_cols <- bt_resolve_cols(y, y_cols)
  dir <- match(direction, c("backward", "forward", "nearest")) - 1L
  bt_as_data_table(.Call(
    bt_rolling_join_, x, y,
    as.integer(match(exact, names(x))),
    as.integer(match(exact, names(y))),
    as.integer(match(x_roll, names(x))),
    as.integer(match(y_roll, names(y))),
    as.integer(dir),
    as.numeric(tolerance),
    as.integer(match(y_cols, names(y)))
  ))
}

# Errors when a shared column name carries different classes across inputs.
bt_assert_no_type_conflicts <- function(dfs) {
  seen <- list()
  for (i in seq_along(dfs)) {
    df <- dfs[[i]]
    for (nm in names(df)) {
      cls <- class(df[[nm]])[1L]
      prior <- seen[[nm]]
      if (is.null(prior)) {
        seen[[nm]] <- list(class = cls, input = i)
      } else if (!identical(prior$class, cls)) {
        stop(sprintf(
          "Column `%s` has conflicting types across inputs: `%s` (input %d) vs `%s` (input %d). Use typeconflict = \"coerce\" to allow automatic coercion.",
          nm, prior$class, prior$input, cls, i
        ), call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

bt_renamecols_old_name <- function(expr, enclos) {
  if (is.symbol(expr)) {
    return(as.character(expr))
  }
  value <- eval(expr, envir = parent.frame(), enclos = enclos)
  if (!is.character(value) || length(value) != 1L) {
    stop("Rename targets must be bare column names or single strings.", call. = FALSE)
  }
  value
}

# Vectorized last-observation-carried-forward for atomic vectors.
bt_locf <- function(x) {
  ok <- !is.na(x)
  idx <- cumsum(ok)
  fill_pos <- which(!ok & idx > 0L)
  if (length(fill_pos) > 0L) {
    x[fill_pos] <- x[ok][idx[fill_pos]]
  }
  x
}

bt_nocb <- function(x) rev(bt_locf(rev(x)))

bt_col_expr <- function(expr, data) {
  eval(expr, envir = data, enclos = parent.frame())
}

bt_resolve_cols <- function(data, cols, allow_null = FALSE) {
  nms <- names(data)

  if (is.null(cols)) {
    if (allow_null) {
      return(nms)
    }
    stop("`cols` must not be NULL.", call. = FALSE)
  }

  if (!is.character(cols)) {
    stop("Column specification must be a character vector.", call. = FALSE)
  }

  missing_cols <- setdiff(cols, nms)
  if (length(missing_cols) > 0L) {
    stop(
      sprintf("Unknown columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  cols
}

bt_recycle_flag <- function(x, n, arg) {
  if (length(x) == 1L) {
    rep(x, n)
  } else if (length(x) == n) {
    x
  } else {
    stop(sprintf("`%s` must have length 1 or %s.", arg, n), call. = FALSE)
  }
}

bt_mode <- function(x) {
  cls <- class(x)
  if (length(cls) == 0L) {
    typeof(x)
  } else {
    cls[[1L]]
  }
}

bt_top_values <- function(x, n = 3L) {
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- "<NA>"
  tab <- sort(table(x_chr), decreasing = TRUE)
  if (length(tab) == 0L) {
    return("")
  }
  top <- utils::head(tab, n)
  paste(sprintf("%s (%s)", names(top), as.integer(top)), collapse = ", ")
}

bt_distinct_n <- function(x) {
  length(unique(x, incomparables = FALSE))
}

bt_need_data_table <- function(feature) {
  stop(sprintf("%s is not wired to the basetable engine yet.", feature), call. = FALSE)
}

bt_is_blank <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  is.na(x) | trimws(as.character(x)) == ""
}

bt_clean_names <- function(nms, method = c("unique", "universal", "minimal")) {
  method <- match.arg(method)

  if (method == "minimal") {
    return(nms)
  }

  cleaned <- tolower(nms)
  cleaned <- gsub("[^[:alnum:]]+", "_", cleaned)
  cleaned <- gsub("^_+|_+$", "", cleaned)
  cleaned[cleaned == ""] <- "x"

  if (method == "universal") {
    cleaned <- make.names(cleaned, unique = FALSE)
  }

  make.unique(cleaned, sep = "_")
}

bt_key_expr <- function(data, by) {
  by <- bt_resolve_cols(data, by)
  interaction(data[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
}

bt_order_data <- function(df, by, decreasing = FALSE, na.last = TRUE) {
  by <- bt_resolve_cols(df, by)
  decreasing <- bt_recycle_flag(decreasing, length(by), "decreasing")
  ord <- do.call(
    order,
    c(df[by], list(decreasing = decreasing, na.last = na.last, method = "radix"))
  )
  df[ord, , drop = FALSE]
}

bt_eval_in_data <- function(expr, data) {
  eval(expr, envir = bt_data_mask(data), enclos = parent.frame())
}

# Opcodes for the native expression stack machine (bt_expr_).
bt_expr_opcodes <- c(
  col = 1L, const = 2L, true = 3L, false = 4L, na = 5L,
  `+` = 10L, `-` = 11L, `*` = 12L, `/` = 13L, `^` = 14L, `%%` = 15L,
  `<` = 20L, `<=` = 21L, `>` = 22L, `>=` = 23L, `==` = 24L, `!=` = 25L,
  `&` = 30L, `|` = 31L,
  neg = 40L, `!` = 41L, pos = 42L,
  ifelse = 50L
)

# Compile a substitute()d call to postfix bytecode for bt_expr_, or return NULL
# when the expression uses anything the kernel does not implement (string ops,
# function calls other than ifelse(), variables that are not numeric/logical
# columns, ...), so the caller can fall back to eval(). `df` is the data frame
# the expression will run against; only its names and column types are used.
bt_compile_expr <- function(expr, df) {
  colnames <- names(df)
  numeric_col <- vapply(df, function(x) is.numeric(x) || is.logical(x), logical(1))
  code <- integer(0)
  args <- integer(0)
  consts <- numeric(0)
  ok <- TRUE

  emit <- function(op, arg = 0L) {
    code[[length(code) + 1L]] <<- op
    args[[length(args) + 1L]] <<- arg
  }
  fail <- function() {
    ok <<- FALSE
    NULL
  }

  walk <- function(e) {
    if (!ok) return(invisible())
    if (is.symbol(e)) {
      pos <- match(as.character(e), colnames)
      if (is.na(pos) || !numeric_col[[pos]]) return(fail())
      return(emit(bt_expr_opcodes[["col"]], pos - 1L))
    }
    if (is.logical(e) && length(e) == 1L) {
      if (is.na(e)) return(emit(bt_expr_opcodes[["na"]]))
      return(emit(bt_expr_opcodes[[if (e) "true" else "false"]]))
    }
    if (is.numeric(e) && length(e) == 1L && !is.na(e)) {
      consts[[length(consts) + 1L]] <<- as.numeric(e)
      return(emit(bt_expr_opcodes[["const"]], length(consts) - 1L))
    }
    if (!is.call(e)) return(fail())

    head <- as.character(e[[1L]])
    nargs <- length(e) - 1L

    if (head == "(" && nargs == 1L) {
      return(walk(e[[2L]]))
    }
    if (head %in% c("+", "-") && nargs == 1L) {
      walk(e[[2L]])
      return(emit(bt_expr_opcodes[[if (head == "-") "neg" else "pos"]]))
    }
    if (head == "!" && nargs == 1L) {
      walk(e[[2L]])
      return(emit(bt_expr_opcodes[["!"]]))
    }
    if (head == "ifelse" && nargs == 3L && is.null(names(e))) {
      walk(e[[2L]]); walk(e[[3L]]); walk(e[[4L]])
      return(emit(bt_expr_opcodes[["ifelse"]]))
    }
    if (nargs == 2L && head %in% c("+", "-", "*", "/", "^", "%%",
                                   "<", "<=", ">", ">=", "==", "!=", "&", "|")) {
      walk(e[[2L]])
      walk(e[[3L]])
      return(emit(bt_expr_opcodes[[head]]))
    }
    fail()
  }

  walk(expr)
  if (!ok || length(code) == 0L) {
    return(NULL)
  }
  list(code = as.integer(code), args = as.integer(args), consts = as.numeric(consts))
}

# Evaluate a row predicate: compile to the native kernel when possible, else
# fall back to eval() in the data mask. A logical result comes back with NA
# folded to FALSE (row filtering treats NA as "drop"); the caller still
# validates type and length.
bt_eval_predicate <- function(expr, df, env) {
  plan <- bt_compile_expr(expr, df)
  if (!is.null(plan)) {
    return(.Call(bt_expr_, df, plan$code, plan$args, plan$consts, TRUE))
  }
  val <- eval(expr, envir = bt_data_mask(df, env), enclos = env)
  if (is.logical(val) && anyNA(val)) val[is.na(val)] <- FALSE
  val
}

bt_eval_logical <- function(expr, data, n) {
  value <- bt_eval_in_data(expr, data)
  if (!is.logical(value) || length(value) != n) {
    stop("Expression must evaluate to a logical vector with one value per row.", call. = FALSE)
  }
  value[is.na(value)] <- FALSE
  value
}

bt_split_by <- function(data, by, drop = FALSE, keepby = FALSE) {
  df <- bt_as_data_frame(data)
  by <- bt_resolve_cols(df, by)
  group_info <- bt_engine_groups(df, by)
  groups <- bt_group_rows(group_info$id)
  out <- lapply(groups, function(idx) {
    piece <- bt_engine_subset(df, rows = idx)
    if (!isTRUE(keepby)) piece <- bt_engine_subset(piece, cols = setdiff(names(piece), by))
    piece
  })
  keys <- df[group_info$first, by, drop = FALSE]
  names(out) <- apply(keys, 1L, paste, collapse = ".")
  out
}

bt_group_keys <- function(data, by) {
  df <- bt_as_data_frame(data)
  by <- bt_resolve_cols(df, by)
  unique(df[, by, drop = FALSE])
}

bt_col_positions <- function(data, cols, allow_null = FALSE) {
  cols <- bt_resolve_cols(data, cols, allow_null = allow_null)
  match(cols, names(data))
}

bt_engine_subset <- function(data, rows = NULL, cols = NULL) {
  df <- bt_as_data_frame(data)
  col_pos <- if (is.null(cols)) NULL else bt_col_positions(df, cols)
  bt_as_data_table(.Call(bt_subset_, df, rows, col_pos))
}

bt_engine_order <- function(data, by, decreasing = FALSE, na.last = TRUE) {
  df <- bt_as_data_frame(data)
  by_pos <- bt_col_positions(df, by)
  decreasing <- bt_recycle_flag(decreasing, length(by_pos), "decreasing")
  bt_as_data_table(.Call(
    bt_order_, df, by_pos, as.logical(decreasing), isTRUE(na.last),
    as.integer(bt_default_threads())
  ))
}

bt_engine_unique <- function(data, by = NULL, keep_all = FALSE) {
  df <- bt_as_data_frame(data)
  by_pos <- if (is.null(by)) seq_along(df) else bt_col_positions(df, by)
  bt_as_data_table(.Call(bt_unique_, df, by_pos, isTRUE(keep_all)))
}

bt_engine_duplicated <- function(data, by = NULL, from_last = FALSE) {
  df <- bt_as_data_frame(data)
  by_pos <- if (is.null(by)) seq_along(df) else bt_col_positions(df, by)
  .Call(bt_duplicated_, df, by_pos, isTRUE(from_last))
}

bt_engine_count <- function(data, by, name = "n") {
  df <- bt_as_data_frame(data)
  by_pos <- bt_col_positions(df, by)
  bt_as_data_table(.Call(bt_count_, df, by_pos, as.character(name)))
}

bt_engine_groups <- function(data, by) {
  df <- bt_as_data_frame(data)
  by_pos <- bt_col_positions(df, by)
  .Call(bt_group_id_, df, by_pos)
}

bt_aggregate_fun_name <- function(expr, value) {
  if (is.character(value) && length(value) == 1L) {
    return(value)
  }
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (nm %in% c("sum", "mean", "min", "max", "var", "sd", "n", "length")) {
      return(nm)
    }
  }
  NULL
}

bt_engine_aggregate <- function(data, by, value, fun, na.rm = FALSE) {
  df <- bt_as_data_frame(data)
  by_pos <- bt_col_positions(df, by)
  value_pos <- bt_col_positions(df, value)
  bt_as_data_table(.Call(
    bt_group_agg_, df, by_pos, value_pos, fun, isTRUE(na.rm),
    as.integer(bt_default_threads())
  ))
}

bt_engine_match_mask <- function(x, y, by) {
  x_df <- bt_as_data_frame(x)
  y_df <- bt_as_data_frame(y)
  x_by <- bt_col_positions(x_df, by)
  y_by <- bt_col_positions(y_df, by)
  .Call(bt_match_mask_, x_df, y_df, x_by, y_by)
}

bt_set_row_names <- function(x, n) {
  if (length(x) == 1L) {
    rep(x, n)
  } else if (length(x) == n) {
    x
  } else {
    stop("Length mismatch.", call. = FALSE)
  }
}
