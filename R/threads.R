#' Set data.table thread count for the session
#'
#' Wrapper around [data.table::setDTthreads()] that makes thread control
#' available from `basetable` without exposing it on every verb.
#'
#' @param threads Integer thread count, or `NULL` to reread environment
#'   settings.
#' @param restore_after_fork Whether to restore multithreading after a fork.
#' @param percent Percentage of detected logical CPUs to use.
#' @param throttle Passed through to `data.table::setDTthreads()`.
#'
#' @return The previous thread count.
#' @export
setthreads <- function(threads = NULL, restore_after_fork = NULL, percent = NULL, throttle = NULL) {
  if (!is.null(percent)) {
    data.table::setDTthreads(
      percent = percent,
      restore_after_fork = restore_after_fork,
      throttle = throttle
    )
  } else {
    data.table::setDTthreads(
      threads = threads,
      restore_after_fork = restore_after_fork,
      throttle = throttle
    )
  }
}
