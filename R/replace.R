#' @include ListMatrix-class.R
NULL

#' Replace elements of a [ListMatrix] object with a value where a condition is met.
#'
#' @param x A [ListMatrix] object.
#' @param cond_mat A logical matrix. It should have the same dimensions as the `x`
#' @param value A value to replace the elements of `x` with, where `cond_mat` is `TRUE`.
#'
#' @rdname replace_if_true
#' @export
setGeneric("replace_if_true", function(x, cond_mat, value) {
  standardGeneric("replace_if_true")
})
#' @rdname replace_if_true
setMethod(
  "replace_if_true",
  signature(x = "ListMatrix", cond_mat = "matrix"),
  function(x, cond_mat, value) {
    stopifnot(all(dim(x) == dim(cond_mat)))
    stopifnot(is.logical(cond_mat))

    x@.Data <- lapply(x@.Data, function(mat) {
      mat[cond_mat] <- value
      mat
    })
    x
  }
)
