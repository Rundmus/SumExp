#' An light S4 class for the matrices with additional information for columns and rows
#'
#' @slot matrices A list with matrices. All matrices must have the same dimensions and
#'   row/column names.
#' @slot col_df A data frame with additional information for columns
#' @slot row_df A data frame with additional information for rows
#' @slot metadata A `list` with any other additional information
#'
#' @exportClass SumExp
#' @export
SumExp <- setClass(
  "SumExp",
  contains = "ListMatrix",
  slots = c(
    col_df = "data.frame",
    row_df = "data.frame",
    metadata = "list"
  )
)

setMethod(
  "initialize", "SumExp",
  function(.Object,
           matrices = list(),
           col_df = data.frame(),
           row_df = data.frame(),
           metadata = list()) {
    .Object@matrices <- matrices
    .Object@col_df <- col_df
    .Object@row_df <- row_df
    .Object@metadata <- metadata
    validObject(.Object)
    return(.Object)
  }
)

.valid_SumExp_lst_mat <- function(x) {
  if (any(is.null(names(x)))) return("Names of @matrices must be defined")
  if (ncol(x) != nrow(x@col_df)) {
    return("Number of columns in @matrices must be equal to number of rows in @col_df")
  }
  if (any(colnames(x) != rownames(x@col_df))) {
    return("Column names in @matrices must be equal to row names in @col_df")
  }
  if (nrow(x) != nrow(x@row_df)) {
    return("Number of rows in @matrices must be equal to number of rows in @row_df")
  }
  if (any(rownames(x) != rownames(x@row_df))) {
    return("Row names in @matrices must be equal to row names in @row_df")
  }
  NULL
}

.valid_SumExp_col_df <- function(x) {
  if (!is.data.frame(x@col_df)) return("@col_df must be a data frame")
  NULL
}

.valid_SumExp_row_df <- function(x) {
  if (!is.data.frame(x@row_df)) return("@row_df must be a data frame")
  NULL
}

setValidity("SumExp", function(object) {
  er_m <- c(
    .valid_SumExp_lst_mat(object),
    .valid_SumExp_row_df(object),
    .valid_SumExp_col_df(object)
  )
  if (!is.null(er_m)) return(er_m)
  TRUE
})

# Methods --------------------------------------------------------------------------------


#' Methods for `SumExp` objects
#'
#' @param x A `SumExp` object
#' @rdname SumExp-class
#' @export
setGeneric("row_df", function(x) standardGeneric("row_df"))
#' @rdname SumExp-class
#' @export
setMethod("row_df", signature(x = "SumExp"), function(x) x@row_df)
#' @rdname SumExp-class
#' @export
setGeneric("row_df<-", function(x, value) standardGeneric("row_df<-"))
#' @rdname SumExp-class
#' @export
setMethod("row_df<-", signature(x = "SumExp", value = "data.frame"), function(x, value) {
  x@row_df <- value
  x
})

#' @rdname SumExp-class
#' @export
setGeneric("col_df", function(x) standardGeneric("col_df"))
#' @rdname SumExp-class
#' @export
setMethod("col_df", signature(x = "SumExp"), function(x) x@col_df)
#' @rdname SumExp-class
#' @export
setGeneric("col_df<-", function(x, value) standardGeneric("col_df<-"))
#' @rdname SumExp-class
#' @export
setMethod("col_df<-", signature(x = "SumExp", value = "data.frame"), function(x, value) {
  x@col_df <- value
  x
})

#' @rdname SumExp-class
#' @export
setGeneric("metadata", function(x) standardGeneric("metadata"))
#' @rdname SumExp-class
#' @export
setMethod("metadata", signature(x = "SumExp"), function(x) x@metadata)
#' @rdname SumExp-class
#' @export
setGeneric("metadata<-", function(x, value) standardGeneric("metadata<-"))
#' @rdname SumExp-class
#' @export
setMethod("metadata<-", signature(x = "SumExp", value = "list"), function(x, value) {
  x@metadata <- value
  x
})

#' @rdname SumExp-class
#' @export
setGeneric("assay", function(x, i, ...) standardGeneric("assay"))
#' @rdname SumExp-class
#' @export
setMethod("assay", signature(x = "SumExp"), function(x, i, ...) x[[i]])
#' @rdname SumExp-class
#' @export
setGeneric("assay<-", function(x, i, ..., value) standardGeneric("assay<-"))
#' @rdname SumExp-class
#' @export
setMethod("assay<-", signature(x = "SumExp", value = "matrix"), function(x, i, ..., value) {
  stopifnot("`i` must have a single value" = length(i) == 1)
  x[[i]] <- value
  x
})

# Subset ---------------------------------------------------------------------------------

#' @rdname SumExp-class
#' @export
setMethod(
  "[", signature(x = "SumExp"),
  function(x, i, j, ..., drop = FALSE) {
    stopifnot("drop must be FALSE" = !drop)
    data_lst <- lapply(x@matrices, \(.x) {
      .x[i, j, drop = FALSE] |>
        labelled::copy_labels_from(.x)
    })
    col_df <- x@col_df[j, , drop = FALSE] |>
      labelled::copy_labels_from(x@col_df)
    row_df <- x@row_df[i, , drop = FALSE] |>
      labelled::copy_labels_from(x@row_df)
    metadata <- x@metadata
    new("SumExp", matrices = data_lst, col_df = col_df, row_df = row_df, metadata = metadata)
  }
)


# tidyverse ------------------------------------------------------------------------------

#' @rdname SumExp-class
#' @export
setGeneric("as_tibble", function(x, ...) standardGeneric("as_tibble"))
#' @rdname SumExp-class
#' @export
setMethod(
  "as_tibble", signature(x = "SumExp"),
  function(x, ...) {
    col_tbl <- tibble::as_tibble(x@col_df, rownames = ".col_id")
    row_tbl <- tibble::as_tibble(x@row_df, rownames = ".row_id")

    stopifnot("Names of @matrices must be unique" = anyDuplicated(names(x@matrices)) == 0)
    mx <- lapply(names(x@matrices), \(ii) {
      .x <- x@matrices[[ii]]
      tibble::as_tibble(.x, rownames = ".row_id") |>
        tidyr::pivot_longer(cols = -c(.row_id), names_to = ".col_id", values_to = ii)
    })
    mx <- purrr::reduce(mx, dplyr::full_join, by = c(".row_id", ".col_id"))

    mx |>
      dplyr::left_join(row_tbl, by = ".row_id") |>
      dplyr::left_join(col_tbl, by = ".col_id")
  }
)

#' @rdname SumExp-class
#' @export
setGeneric("ggplot",
           function(data, mapping = aes(), ..., environment = parent.frame()) {
             standardGeneric("ggplot")
           })
#' @rdname SumExp-class
#' @export
setMethod(
  "ggplot", signature(data = "SumExp"),
  function(data, mapping = aes(), ..., environment = parent.frame()) {
    data |>
      as_tibble() |>
      ggplot2::ggplot(mapping = mapping, ..., environment = environment)
  }
)
