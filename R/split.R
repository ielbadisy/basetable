split <- function(data, by, drop = FALSE, keep.by = TRUE) {
  if (!is.data.frame(data)) {
    return(base::split(x = data, f = by, drop = drop))
  }
  bt_split_by(data, by = by, drop = drop, keepby = keep.by)
}
