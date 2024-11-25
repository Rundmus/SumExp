#' An light S4 class for the matrices with additional information for columns and rows
#'
#' @slot matrices A `ListMatrix` object, a list with the matrices
#' @slot colData A data frame with additional information for columns
#' @slot rowData A data frame with additional information for rows
#' @slot metadata A `list` with any other additional information
#'
#' @exportClass SumExp
SumExp <- setClass(
  "SumExp",
  contains = "ListMatrix",
  slots = c(
    colData = "data.frame",
    rowData = "data.frame",
    metadata = "list"
  )
)

setMethod(
  "initialize", "SumExp",
  function(.Object,
           matrices = list(),
           colData = data.frame(),
           rowData = data.frame(),
           metadata = list()) {
    .Object@matrices <- matrices
    .Object@colData <- colData
    .Object@rowData <- rowData
    .Object@metadata <- metadata
    validObject(.Object)
    return(.Object)
  }
)

.valid_SumExp_lst_mat <- function(x) {
  if (any(is.null(names(x)))) return("Names of @matrices must be defined")
  if (ncol(x) != nrow(x@colData)) {
    return("Number of columns in @matrices must be equal to number of rows in @colData")
  }
  if (any(colnames(x) != rownames(x@colData))) {
    return("Column names in @matrices must be equal to row names in @colData")
  }
  if (nrow(x) != nrow(x@rowData)) {
    return("Number of rows in @matrices must be equal to number of rows in @rowData")
  }
  if (any(rownames(x) != rownames(x@rowData))) {
    return("Row names in @matrices must be equal to row names in @rowData")
  }
  NULL
}

.valid_SumExp_colData <- function(x) {
  if (!is.data.frame(x@colData)) return("@colData must be a data frame")
  NULL
}

.valid_SumExp_rowData <- function(x) {
  if (!is.data.frame(x@rowData)) return("@rowData must be a data frame")
  NULL
}

setValidity("SumExp", function(object) {
  er_m <- c(
    .valid_SumExp_lst_mat(object),
    .valid_SumExp_rowData(object),
    .valid_SumExp_colData(object)
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
setGeneric("rowData", function(x) standardGeneric("rowData"))
#' @rdname SumExp-class
#' @export
setMethod("rowData", signature(x = "SumExp"), function(x) x@rowData)
#' @rdname SumExp-class
#' @export
setGeneric("rowData<-", function(x, value) standardGeneric("rowData<-"))
#' @rdname SumExp-class
#' @export
setMethod("rowData<-", signature(x = "SumExp", value = "data.frame"), function(x, value) {
  x@rowData <- value
  x
})

#' @rdname SumExp-class
#' @export
setGeneric("colData", function(x) standardGeneric("colData"))
#' @rdname SumExp-class
#' @export
setMethod("colData", signature(x = "SumExp"), function(x) x@colData)
#' @rdname SumExp-class
#' @export
setGeneric("colData<-", function(x, value) standardGeneric("colData<-"))
#' @rdname SumExp-class
#' @export
setMethod("colData<-", signature(x = "SumExp", value = "data.frame"), function(x, value) {
  x@colData <- value
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
  "[", signature(x = "SumExp", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ..., drop = FALSE) {
    stopifnot("drop must be FALSE" = !drop)
    data_lst <- lapply(x@matrices, \(.x) {
      .x[i, j, drop = FALSE] |>
        labelled::copy_labels_from(.x)
    })
    colData <- x@colData[j, , drop = FALSE] |>
      labelled::copy_labels_from(x@colData)
    rowData <- x@rowData[i, , drop = FALSE] |>
      labelled::copy_labels_from(x@rowData)
    metadata <- x@metadata
    new("SumExp", matrices = data_lst, colData = colData, rowData = rowData, metadata = metadata)
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
    col_tbl <- tibble::as_tibble(x@colData, rownames = ".col_id")
    row_tbl <- tibble::as_tibble(x@rowData, rownames = ".row_id")

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
