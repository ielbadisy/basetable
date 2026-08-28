#' Read a delimited text file
#'
#' A from-scratch delimited-file reader written in C++: the file is memory
#' mapped, scanned once for row boundaries (RFC 4180 quoting), type-guessed
#' from a bounded sample, and materialised into typed vectors. Numeric columns
#' are filled by a thread pool; character columns are built on the R thread.
#'
#' With `lazy = TRUE` the integer and double columns are returned as ALTREP
#' vectors that parse their column only the first time it is touched, which
#' makes "read a few columns from a wide file" close to free.
#'
#' @param file Path to a delimited text file. `.gz` inputs are decompressed to
#'   a temporary file first.
#' @param delim Field delimiter. `NULL` (default) sniffs the first line for
#'   one of `,` `\t` `;` `|` and a space.
#' @param header Logical; does the first row hold column names?
#' @param col_names Optional character vector of names, used only when
#'   `header = FALSE`. Defaults to `V1`, `V2`, ...
#' @param col_types `NULL` to guess, or a character vector of
#'   `"logical"`, `"integer"`, `"double"`, `"character"`, `"skip"`, `"guess"`.
#'   Length 1 is recycled; otherwise it must have one entry per column.
#' @param col_select Optional integer positions or character names of the
#'   columns to keep. Other columns are not parsed.
#' @param na Character vector of strings to read as `NA`.
#' @param quote Quoting character.
#' @param comment Lines beginning with this one-character string are skipped
#'   (`""` disables).
#' @param trim_ws Logical; strip leading/trailing spaces and tabs from unquoted
#'   fields.
#' @param skip Number of raw lines to discard before reading.
#' @param n_max Maximum number of data rows to read (`Inf` for all).
#' @param guess_max Number of rows sampled for type guessing.
#' @param lazy Logical; return numeric columns as lazily-parsed ALTREP vectors.
#' @param n_threads Number of worker threads for the eager numeric fill.
#' @param as Return `"data.frame"` (default) or `"data.table"`.
#'
#' @return A data.frame (or data.table).
#' @export
#' @examples
#' p <- tempfile(fileext = ".csv")
#' write.csv(head(iris), p, row.names = FALSE)
#' btread(p)
btread <- function(file,
                    delim = NULL,
                    header = TRUE,
                    col_names = NULL,
                    col_types = NULL,
                    col_select = NULL,
                    na = c("NA", ""),
                    quote = "\"",
                    comment = "",
                    trim_ws = FALSE,
                    skip = 0,
                    n_max = Inf,
                    guess_max = 10000,
                    lazy = FALSE,
                    n_threads = bt_default_threads(),
                    as = c("data.frame", "data.table")) {
  as <- match.arg(as)
  stopifnot(length(file) == 1L, is.character(file))
  file <- path.expand(file)
  if (!file.exists(file)) stop("btread: file not found: ", file, call. = FALSE)

  if (grepl("\\.gz$", file, ignore.case = TRUE)) {
    file <- bt_gunzip_to_tmp(file)
    on.exit(unlink(file), add = TRUE)
  }

  delim_chr <- if (is.null(delim)) "" else {
    if (identical(delim, "\t")) "\\t" else as.character(delim)
  }
  comment <- if (is.null(comment) || !nzchar(comment)) "" else substr(comment, 1L, 1L)
  n_max <- if (is.infinite(n_max)) NULL else as.numeric(n_max)

  header_names <- NULL
  if (!is.null(col_select) && is.character(col_select) ||
      (!is.null(col_types) && length(col_types) > 1L && !is.null(names(col_types)))) {
    header_names <- bt_peek_header(file, delim_chr, quote, skip, header)
  }

  col_select_int <- NULL
  if (!is.null(col_select)) {
    col_select_int <- if (is.character(col_select)) {
      m <- match(col_select, header_names)
      if (anyNA(m)) stop("btread: unknown col_select name(s): ",
                         paste(col_select[is.na(m)], collapse = ", "), call. = FALSE)
      as.integer(m)
    } else as.integer(col_select)
  }

  ct <- NULL
  if (!is.null(col_types)) {
    valid <- c("logical", "integer", "double", "character", "skip", "guess")
    ct <- as.character(col_types)
    if (!is.null(names(col_types)) && !is.null(header_names)) {
      full <- rep("guess", length(header_names))
      idx <- match(names(col_types), header_names)
      full[idx[!is.na(idx)]] <- ct[!is.na(idx)]
      ct <- full
    }
    if (!all(ct %in% valid))
      stop("btread: col_types must be one of ", paste(valid, collapse = ", "),
           call. = FALSE)
  }

  out <- .Call(btread_,
               file,
               delim_chr,
               substr(quote, 1L, 1L),
               comment,
               isTRUE(header),
               isTRUE(trim_ws),
               as.numeric(skip),
               n_max,
               as.character(na),
               as.integer(n_threads),
               as.numeric(guess_max),
               col_names,
               ct,
               col_select_int,
               isTRUE(lazy))

  if (as == "data.table") {
    out <- bt_as_data_table(out)
  }
  out
}

#' Write a data frame to a delimited text file
#'
#' A threaded C++ writer: each column is resolved to a plain C array on the R
#' thread, then disjoint row ranges are formatted into private buffers and
#' flushed in order.
#'
#' @param x A data.frame or data.table.
#' @param file Output path.
#' @param delim Field delimiter.
#' @param na String to write for `NA`.
#' @param col_names Logical; write a header row?
#' @param quote Quoting character; a field is quoted only if it contains the
#'   delimiter, the quote character, or a newline.
#' @param digits Significant digits for double columns (passed to `%g`).
#' @param append Logical; append to `file` instead of overwriting (no header is
#'   written when appending).
#' @param n_threads Number of worker threads.
#'
#' @return `file`, invisibly.
#' @export
#' @examples
#' p <- tempfile(fileext = ".csv")
#' btwrite(head(iris), p)
#' btread(p)
btwrite <- function(x,
                     file,
                     delim = ",",
                     na = "NA",
                     col_names = TRUE,
                     quote = "\"",
                     digits = 15,
                     append = FALSE,
                     n_threads = bt_default_threads()) {
  stopifnot(is.data.frame(x))
  file <- path.expand(file)
  x <- as.data.frame(x)

  # hand off types the C writer does not format itself
  for (j in seq_along(x)) {
    col <- x[[j]]
    if (inherits(col, c("Date", "POSIXct", "POSIXlt", "difftime")) ||
        (!is.null(attr(col, "class")) && !is.factor(col)))
      x[[j]] <- as.character(col)
  }

  invisible(.Call(btwrite_,
                  x,
                  file,
                  substr(delim, 1L, 1L),
                  substr(quote, 1L, 1L),
                  as.character(na),
                  as.integer(digits),
                  isTRUE(col_names),
                  isTRUE(append),
                  as.integer(n_threads)))
}

# --- internal helpers -------------------------------------------------------

bt_default_threads <- function() {
  n <- getOption("basetable.threads", NA_integer_)
  if (is.na(n) || n < 1L) n <- max(1L, parallel::detectCores(logical = TRUE))
  as.integer(min(n, 8L))
}

bt_peek_header <- function(file, delim_chr, quote, skip, header) {
  con <- file(file, "rt")
  on.exit(close(con))
  ln <- readLines(con, n = skip + 1L, warn = FALSE)
  if (!length(ln)) return(character())
  line <- ln[length(ln)]
  sep <- if (identical(delim_chr, "")) {
    cand <- c(",", "\t", ";", "|", " ")
    counts <- vapply(cand, function(s) lengths(regmatches(line, gregexpr(s, line, fixed = TRUE))), integer(1))
    hit <- which(counts > 0)
    if (length(hit)) cand[hit[which.max(counts[hit])]] else ","
  } else if (identical(delim_chr, "\\t")) "\t" else delim_chr
  parts <- strsplit(line, sep, fixed = TRUE)[[1]]
  parts <- gsub(paste0("^", quote, "|", quote, "$"), "", parts)
  if (isTRUE(header)) parts else paste0("V", seq_along(parts))
}

bt_gunzip_to_tmp <- function(file) {
  tmp <- tempfile(fileext = ".csv")
  incon <- gzfile(file, "rb"); outcon <- file(tmp, "wb")
  on.exit({ close(incon); close(outcon) })
  repeat {
    chunk <- readBin(incon, "raw", n = 1024L * 1024L)
    if (!length(chunk)) break
    writeBin(chunk, outcon)
  }
  tmp
}
