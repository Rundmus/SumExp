#' @include SumExp-class.R
NULL

#' Coerce a SumExp object to a SummarizedExperiment object
#'
#' @param x A [`SumExp`] object
#' @param ... Not used
#' @returns A [`SummarizedExperiment`] object with the row and column data frames as well as the
#    matrices. Each matrix '  in the [`SumExp`] object is stored in the `assays` slot of the
#    [`SummarizedExperiment`] object.
#' @export
setGeneric("as_SummarizedExperiment", function(x, ...) standardGeneric("as_SummarizedExperiment"))
#' @rdname as_SummarizedExperiment
#' @export
setMethod(
  "as_SummarizedExperiment", signature(x = "SumExp"),
  function(x, ...) {
    SummarizedExperiment::SummarizedExperiment(
      assays = do.call(S4Vectors::SimpleList, as.list(x)),
      rowData = x@row_df,
      colData = x@col_df,
      metadata = x@metadata
    )
  }
)
