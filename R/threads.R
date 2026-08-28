#' Set basetable thread count for the session
#'
#' Controls the default thread count used by basetable's native reader and
#' writer when a verb does not receive `n_threads` explicitly.
#'
#' @param threads Integer thread count, or `NULL` to reread environment
#'   settings.
#' @param percent Percentage of detected logical CPUs to use.
#' @param restore_after_fork Ignored; kept for API compatibility.
#' @param throttle Ignored; kept for API compatibility.
#'
#' @return The previous thread count.
#' @export
setthreads <- function(threads = NULL, restore_after_fork = NULL, percent = NULL, throttle = NULL) {
  old <- getOption("basetable.threads", bt_default_threads())
  if (!is.null(percent)) {
    cores <- max(1L, parallel::detectCores(logical = TRUE))
    threads <- max(1L, floor(cores * as.numeric(percent) / 100))
  } else if (is.null(threads)) {
    threads <- max(1L, parallel::detectCores(logical = TRUE))
  } else {
    threads <- as.integer(threads)
  }
  options(basetable.threads = as.integer(max(1L, threads)))
  invisible(old)
}
