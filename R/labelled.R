#' Copy labels handling nested lists
#'
#' [labelled::copy_labels()] does not handle nested lists. This function is a workaround.
#' @inheritParams labelled::copy_labels
copy_labels_list <- function(from, to) {
  if (!is.list(from)) return(labelled::copy_labels(from = from, to = to))
  labelled::label_attribute(to) <- labelled::label_attribute(from)
  lapply(1:length(from), \(ii) {
    copy_labels_list(from[[ii]], to[[ii]])
  })
}

#' Subset and copy labels handling nested lists
#'
#' @param from A data frame
#' @param idx Index to subset `from`
subset_copy_labels.data.frame <- function(from, idx) {
  out <- from[idx, , drop = FALSE]
  # Copy labels
  is_list <- sapply(from, is.list)
  out[is_list] <- lapply(which(is_list), \(kk) {
    copy_labels_list(from[[kk]][idx], out[[kk]])
  })
  out[!is_list] <- labelled::copy_labels(from = from[!is_list], to = out[!is_list])
  out
}
