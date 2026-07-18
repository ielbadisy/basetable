nearesttext <- function(x, candidates, ignore_case = FALSE) {
  if (length(candidates) == 0L) {
    stop("`candidates` must not be empty.", call. = FALSE)
  }

  dist <- textdist(x, candidates, ignore_case = ignore_case)
  idx <- apply(dist, 1L, function(row) {
    if (all(is.na(row))) {
      return(NA_integer_)
    }

    row[is.na(row)] <- Inf
    which.min(row)
  })

  out <- rep(NA_character_, length(idx))
  keep <- !is.na(idx)
  out[keep] <- candidates[idx[keep]]
  out
}
