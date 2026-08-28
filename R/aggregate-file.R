#' Grouped aggregation straight off a delimited file
#'
#' A fused one-pass group-by: [btread()]'s reader machinery
#' (memory map, parallel row index) is used to scan the file once, extracting
#' only the grouping and value fields, grouping, and reducing. The grouping and
#' value columns are never materialised as R vectors, so on a wide file this is
#' typically 2-3x faster end-to-end than reading the whole file and grouping it.
#'
#' @param file Path to a delimited text file (`.gz` supported).
#' @param by Character vector of grouping column names (or 1-based positions).
#' @param value Character vector of numeric column names to aggregate (or
#'   positions). Defaults to every column not in `by`.
#' @param fun One or more of `"sum"`, `"mean"`, `"var"`, `"sd"`, `"min"`,
#'   `"max"`, `"n"`. With several, output columns are named `<value>_<fun>`.
#' @param where Optional character vector of simple row filters applied *in the
#'   same pass* (predicate pushdown), each `"<col> <op> <literal>"` with `op`
#'   one of `< <= > >= == !=`. String literals may be quoted. All are AND-ed.
#' @param na.rm Logical; drop `NA` values before reducing.
#' @param delim,quote,comment,header,skip,n_max,na See [btread()].
#' @param n_threads Worker threads.
#' @param as Return `"data.frame"` (default) or `"data.table"`.
#'
#' @return A data.frame (or data.table): the `by` columns, then one column per
#'   `value` x `fun`.
#' @export
#' @examples
#' p <- tempfile(fileext = ".csv")
#' write.csv(data.frame(g = rep(letters[1:3], 4), x = 1:12), p, row.names = FALSE)
#' bt_aggregate(p, by = "g", value = "x", fun = c("sum", "mean"))
bt_aggregate <- function(file,
                         by,
                         value = NULL,
                         fun = "sum",
                         where = NULL,
                         na.rm = TRUE,
                         delim = NULL,
                         quote = "\"",
                         comment = "",
                         header = TRUE,
                         skip = 0,
                         n_max = Inf,
                         na = c("NA", ""),
                         n_threads = bt_default_threads(),
                         as = c("data.frame", "data.table")) {
  as <- match.arg(as)
  stopifnot(length(file) == 1L, is.character(file))
  file <- path.expand(file)
  if (!file.exists(file)) stop("bt_aggregate: file not found: ", file, call. = FALSE)
  if (grepl("\\.gz$", file, ignore.case = TRUE)) {
    file <- bt_gunzip_to_tmp(file); on.exit(unlink(file), add = TRUE)
  }

  valid <- c("sum", "mean", "var", "sd", "min", "max", "n")
  fun <- as.character(fun)
  if (!all(fun %in% valid))
    stop("bt_aggregate: fun must be one of ", paste(valid, collapse = ", "), call. = FALSE)

  delim_chr <- if (is.null(delim)) "" else if (identical(delim, "\t")) "\\t" else as.character(delim)
  comment <- if (is.null(comment) || !nzchar(comment)) "" else substr(comment, 1L, 1L)
  n_max <- if (is.infinite(n_max)) NULL else as.numeric(n_max)

  peek <- bt_peek_sample(file, delim_chr, quote, skip, header, comment, n = 200L)
  hdr  <- peek$names
  if (identical(delim_chr, "")) delim_chr <- peek$sep

  resolve <- function(x) {
    if (is.numeric(x)) as.integer(x)
    else {
      m <- match(x, hdr)
      if (anyNA(m)) stop("bt_aggregate: unknown column(s): ",
                         paste(x[is.na(m)], collapse = ", "), call. = FALSE)
      m
    }
  }
  by_idx <- resolve(by)
  only_n <- all(fun == "n")
  if (is.null(value)) {
    value_idx <- if (only_n) by_idx[1] else setdiff(seq_along(hdr), by_idx)
  } else value_idx <- resolve(value)
  if (!length(value_idx)) stop("bt_aggregate: no value columns", call. = FALSE)

  # predicate pushdown: parse "<col> <op> <literal>"
  w_col <- integer(0); w_op <- integer(0); w_kind <- integer(0)
  w_dval <- numeric(0); w_sval <- character(0)
  if (!is.null(where) && length(where)) {
    ops <- c("<=", ">=", "==", "!=", "<", ">")
    code <- c(`<` = 0L, `<=` = 1L, `>` = 2L, `>=` = 3L, `==` = 4L, `!=` = 5L)
    for (w in where) {
      m <- regmatches(w, regexec(
        "^\\s*(\\S+?)\\s*(<=|>=|==|!=|<|>)\\s*(.+?)\\s*$", w))[[1]]
      if (length(m) != 4L) stop("bt_aggregate: cannot parse where clause: ", w, call. = FALSE)
      col <- resolve(m[2]); op <- code[[m[3]]]
      lit <- m[4]
      lit <- sub("^([\"'])(.*)\\1$", "\\2", lit)
      num <- suppressWarnings(as.numeric(lit))
      w_col  <- c(w_col, col)
      w_op   <- c(w_op, op)
      if (is.na(num)) { w_kind <- c(w_kind, 2L); w_dval <- c(w_dval, NA_real_); w_sval <- c(w_sval, lit) }
      else            { w_kind <- c(w_kind, 0L); w_dval <- c(w_dval, num);     w_sval <- c(w_sval, NA_character_) }
    }
  }

  by_kind <- vapply(by_idx, function(j) {
    col <- peek$sample[[j]]
    col <- col[!is.na(col) & nzchar(col)]
    if (!length(col)) return(2L)                       # all-missing -> string
    if (all(grepl("^[+-]?[0-9]+$", col)) &&
        all(abs(suppressWarnings(as.numeric(col))) < 2^31)) 0L
    else if (!anyNA(suppressWarnings(as.numeric(col)))) 1L
    else 2L
  }, integer(1))

  out <- .Call(bt_agg_,
               file, delim_chr, substr(quote, 1L, 1L), comment,
               isTRUE(header), as.numeric(skip), n_max, as.character(na),
               as.integer(n_threads),
               as.integer(by_idx), as.integer(by_kind),
               hdr[by_idx],
               as.integer(value_idx), hdr[value_idx],
               fun, isTRUE(na.rm),
               as.integer(w_col), as.integer(w_op), as.integer(w_kind),
               as.numeric(w_dval), as.character(w_sval))

  # count columns are whole numbers -> return integer where it fits
  nk <- length(by_idx); nvv <- length(value_idx); nff <- length(fun)
  for (fi in which(fun == "n")) for (v in seq_len(nvv)) {
    j <- nk + (v - 1L) * nff + fi
    if (j <= length(out) && is.numeric(out[[j]]) && max(out[[j]], 0) < 2147483647)
      out[[j]] <- as.integer(out[[j]])
  }
  if (only_n && is.null(value)) names(out)[length(out)] <- "n"

  if (as == "data.table") {
    out <- bt_as_data_table(out)
  }
  out
}

#' Count rows per group straight off a delimited file
#'
#' One-pass grouped row count. Equivalent to
#' `bt_aggregate(file, by, fun = "n")` with an `n` column.
#'
#' @inheritParams bt_aggregate
#' @return A data.frame (or data.table): the `by` columns and an `n` count.
#' @export
#' @examples
#' p <- tempfile(fileext = ".csv")
#' write.csv(data.frame(g = rep(letters[1:3], 4)), p, row.names = FALSE)
#' bt_count(p, by = "g")
bt_count <- function(file, by, where = NULL, delim = NULL, quote = "\"",
                     comment = "", header = TRUE, skip = 0, n_max = Inf,
                     na = c("NA", ""), n_threads = bt_default_threads(),
                     as = c("data.frame", "data.table")) {
  bt_aggregate(file, by = by, value = NULL, fun = "n", where = where,
               na.rm = TRUE, delim = delim, quote = quote, comment = comment,
               header = header, skip = skip, n_max = n_max, na = na,
               n_threads = n_threads, as = match.arg(as))
}

#' Distinct key combinations straight off a delimited file
#'
#' One-pass unique combinations of `cols` (like `unique(x, by = cols)` but
#' keeping only `cols`).
#'
#' @inheritParams bt_aggregate
#' @param cols Columns whose distinct combinations to return.
#' @return A data.frame (or data.table) of the distinct combinations.
#' @export
bt_distinct <- function(file, cols, where = NULL, delim = NULL, quote = "\"",
                        comment = "", header = TRUE, skip = 0, n_max = Inf,
                        na = c("NA", ""), n_threads = bt_default_threads(),
                        as = c("data.frame", "data.table")) {
  out <- bt_count(file, by = cols, where = where, delim = delim, quote = quote,
                  comment = comment, header = header, skip = skip, n_max = n_max,
                  na = na, n_threads = n_threads, as = "data.frame")
  out[["n"]] <- NULL
  if (match.arg(as) == "data.table") out <- bt_as_data_table(out)
  out
}

#' Frequency table straight off a delimited file
#'
#' One-pass grouped counts plus proportions.
#'
#' @inheritParams bt_aggregate
#' @param sort Logical; order rows by descending count.
#' @return A data.frame (or data.table): the `by` columns, `n`, and `prop`.
#' @export
bt_freq <- function(file, by, where = NULL, sort = TRUE, delim = NULL,
                    quote = "\"", comment = "", header = TRUE, skip = 0,
                    n_max = Inf, na = c("NA", ""),
                    n_threads = bt_default_threads(),
                    as = c("data.frame", "data.table")) {
  out <- bt_count(file, by = by, where = where, delim = delim, quote = quote,
                  comment = comment, header = header, skip = skip, n_max = n_max,
                  na = na, n_threads = n_threads, as = "data.frame")
  out[["prop"]] <- out[["n"]] / sum(out[["n"]])
  if (isTRUE(sort)) out <- bt_engine_order(out, by = "n", decreasing = TRUE)
  row.names(out) <- NULL
  if (match.arg(as) == "data.table") out <- bt_as_data_table(out)
  out
}

# read header + a small row sample for name resolution and key-type inference
bt_peek_sample <- function(file, delim_chr, quote, skip, header, comment, n = 200L) {
  con <- file(file, "rt"); on.exit(close(con))
  raw <- readLines(con, n = skip + n + 1L, warn = FALSE)
  if (skip > 0) raw <- raw[-seq_len(skip)]
  if (nzchar(comment)) raw <- raw[!startsWith(raw, comment)]
  if (!length(raw)) return(list(names = character(), sample = list(), sep = ","))

  sep <- if (identical(delim_chr, "")) {
    cand <- c(",", "\t", ";", "|", " ")
    counts <- vapply(cand, function(s)
      lengths(regmatches(raw[1], gregexpr(s, raw[1], fixed = TRUE))), integer(1))
    hit <- which(counts > 0)
    if (length(hit)) cand[hit[which.max(counts[hit])]] else ","
  } else if (identical(delim_chr, "\\t")) "\t" else delim_chr

  unq <- function(v) gsub(paste0("^", quote, "|", quote, "$"), "", v)
  h1 <- unq(strsplit(raw[1], sep, fixed = TRUE)[[1]])
  if (isTRUE(header)) { nms <- h1; body <- raw[-1] }
  else { nms <- paste0("V", seq_along(h1)); body <- raw }

  cells <- lapply(body, function(l) unq(strsplit(l, sep, fixed = TRUE)[[1]]))
  ncol <- length(nms)
  sample <- lapply(seq_len(ncol), function(j)
    vapply(cells, function(r) if (length(r) >= j) r[[j]] else NA_character_, character(1)))
  list(names = nms, sample = sample, sep = sep)
}
