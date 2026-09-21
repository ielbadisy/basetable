#' Nest rows into a list-column of tables
#'
#' `nest()` collapses the rows of each group into a data frame and stores those
#' data frames in a single list-column, giving one row per group. Groups appear
#' in order of first occurrence. Missing values in `by` form their own group.
#'
#' @param data A data frame.
#' @param by Character vector of grouping columns.
#' @param name Name of the new list-column.
#'
#' @return A `basetable` with the `by` columns and one list-column holding the
#'   remaining columns of each group as a data frame.
#' @seealso [unnest()], [split()]
#' @export
#' @examples
#' nested <- nest(data.frame(g = c("a", "a", "b"), x = 1:3), by = "g")
#' nested
#' nested$data[[1]]
nest <- function(data, by, name = "data") {
  df <- bt_as_data_frame(data)
  by <- bt_resolve_cols(df, by)
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("`name` must be a single non-empty string.", call. = FALSE)
  }
  if (name %in% by) {
    stop("`name` must not be one of the `by` columns.", call. = FALSE)
  }

  rest <- setdiff(names(df), by)
  group_info <- bt_engine_groups(df, by)
  groups <- bt_group_rows(group_info$id)

  pieces <- lapply(groups, function(idx) {
    bt_engine_subset(df, rows = idx, cols = rest)
  })

  out <- bt_as_data_frame(bt_engine_subset(df, rows = group_info$first, cols = by))
  out[[name]] <- unname(pieces)
  bt_as_data_table(out)
}

#' Expand list-columns back into rows
#'
#' `unnest()` is the inverse of [nest()]. Each element of a list-column is
#' expanded into rows, and the other columns are repeated to match. Elements
#' may be data frames (their columns are added) or atomic vectors (the values
#' are stored under the list-column's own name). Rows whose element is empty
#' are dropped.
#'
#' @param data A data frame with one or more list-columns.
#' @param cols Character vector of list-columns to expand. When several are
#'   given, their elements must have matching lengths within each row.
#'
#' @return A `basetable` with one row per element.
#' @seealso [nest()]
#' @export
#' @examples
#' nested <- nest(data.frame(g = c("a", "a", "b"), x = 1:3), by = "g")
#' unnest(nested, "data")
unnest <- function(data, cols) {
  df <- bt_as_data_frame(data)
  cols <- bt_resolve_cols(df, cols)
  not_list <- cols[!vapply(df[cols], is.list, logical(1L))]
  if (length(not_list) > 0L) {
    stop(
      sprintf("Not list-columns: %s", paste(not_list, collapse = ", ")),
      call. = FALSE
    )
  }

  elt_len <- function(x) if (is.data.frame(x)) nrow(x) else length(x)
  lens <- lapply(df[cols], function(col) vapply(col, elt_len, integer(1L)))
  if (length(cols) > 1L && any(vapply(lens[-1L], function(l) any(l != lens[[1L]]), logical(1L)))) {
    stop("List-columns must have matching element lengths.", call. = FALSE)
  }
  lens <- lens[[1L]]

  idx <- rep.int(seq_len(nrow(df)), lens)
  outer <- df[idx, setdiff(names(df), cols), drop = FALSE]
  rownames(outer) <- NULL

  for (col in cols) {
    elts <- df[[col]]
    elts <- elts[lens > 0L]
    if (length(elts) > 0L && all(vapply(elts, is.data.frame, logical(1L)))) {
      inner <- bt_as_data_frame(bt_rbind_fill(elts))
      clash <- intersect(names(inner), names(outer))
      if (length(clash) > 0L) {
        stop(
          sprintf("Unnested columns clash with existing columns: %s",
                  paste(clash, collapse = ", ")),
          call. = FALSE
        )
      }
      outer <- cbind(outer, inner)
    } else {
      outer[[col]] <- if (length(elts) == 0L) logical(0L) else do.call(c, unname(elts))
    }
  }

  bt_as_data_table(outer)
}
