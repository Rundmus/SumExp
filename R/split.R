#' @include SumExp-class.R
NULL

#' Divide into groups
#'
#' @description
#' Split a matrix-like object, e.g. [`SumExp`], into groups based on a factor
#' column-/row-wise.
#'
#' @param x A matrix-like object, e.g. [`SumExp`]
#' @param f A factor vector to split by
#'
#' @returns A list of matrix-like objects
#' @md
#' @examples
#' se <- SumExp::exmpl_se
#' split_rows(se, row_df(se)$color)
#' split_columns(se, rep(1:2, each = ncol(se) / 2))
#' split_columns(exmpl_se, f = col_df(exmpl_se)$type)
#' @export
split_columns <- function(x, f) {
  stopifnot(length(f) == ncol(x))
  f <- as.factor(f)      # To make sure it is a factor
  split(x = seq_len(ncol(x)), f = f, drop = FALSE) |>
    lapply(function(i) x[, i, drop = FALSE])
}
#' @rdname split_columns
#' @export
split_rows <- function(x, f) {
  stopifnot(length(f) == nrow(x))
  f <- as.factor(f)      # To make sure it is a factor
  split(x = seq_len(nrow(x)), f = f, drop = FALSE) |>
    lapply(function(i) x[i, , drop = FALSE])
}
