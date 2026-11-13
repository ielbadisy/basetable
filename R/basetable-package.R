#' Base-Faithful Tabular Data Tools
#'
#' The \pkg{basetable} package provides a compact set of base-style data
#' manipulation and exploratory data analysis helpers backed by a native
#' C++ table engine.
#'
#' @details
#' Core design rules:
#' \itemize{
#' \item exported data verbs take the main data object as their first argument
#' \item grouping is explicit with \code{by =}
#' \item outputs are plain data frames or lists in most cases
#' \item interfaces are intended to work naturally in nested calls and native
#' \code{|>} pipelines
#' }
#'
#' @useDynLib basetable, btread_, btwrite_, bt_agg_, bt_subset_, bt_order_, bt_unique_, bt_duplicated_, bt_count_, bt_group_agg_, bt_match_mask_, bt_group_id_, bt_join_, bt_first_match_, bt_range_join_, bt_rolling_join_, bt_expr_, bt_filter_, bt_rbind_, bt_nest_, bt_unnest_lens_, bt_unnest_frames_
#' @importFrom parallel detectCores
#' @importFrom utils head
#' @keywords internal
"_PACKAGE"
