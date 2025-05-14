#' @include ListMatrix-class.R
NULL

#' Replace elements of a [ListMatrix] object with a value where a condition is met.
#'
#' @param x A [ListMatrix] object.
#' @param cond A logical matrix. It should have the same dimensions as the `x`
#' @param value A value to replace the elements of `x` with, where `cond` is `TRUE`.
#' @param ... Additional arguments. Not used.
#'
#' @returns A [ListMatrix] object with the elements replaced where `cond` is `TRUE`.
#'
#' @export
replace_if_true <- function(x, cond, value, ...) {
  UseMethod("replace_if_true")
}
#' @rdname replace_if_true
#' @method replace_if_true ListMatrix
#' @export
replace_if_true.ListMatrix <- function(x, cond, value, ...) {
  stopifnot(all(dim(x) == dim(cond)))
  stopifnot(is.logical(cond))

  x@.Data <- lapply(x@.Data, function(mat) {
    mat[cond] <- value
    mat
  })
  x
}
