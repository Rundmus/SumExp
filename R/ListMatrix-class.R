#' A class for storing a list of matrices
#'
#' @slot matrices A list with matrices. All matrices must have the same dimensions and
#'   row/column names.
#' @exportClass ListMatrix
#' @export
ListMatrix <- setClass(
  "ListMatrix",
  contains = "list"
)

setMethod(
  "initialize", "ListMatrix",
  function(.Object, ...) {
    .Object@.Data <- list(...)
    validObject(.Object)
    return(.Object)
  }
)

setValidity("ListMatrix", function(object) {
  for(ii in seq_along(object)) {
    mat_ii <- object[[ii]]
    if (!is.matrix(mat_ii)) return("Every element must be a matrix")
    if (ncol(mat_ii) > 0 & is.null(colnames(mat_ii))) {      # Allow matrices with no columns
      return("Column names in all matrices must be defined")
    }
    if (nrow(mat_ii) > 0 & is.null(rownames(mat_ii))) {      # Allow matrices with no rows
      return("Row names in all matrices must be defined")
    }
    if (ii > 1) {
      mat_ii_1 <- object[[ii - 1]]
      if (!identical(dim(mat_ii), dim(mat_ii_1))) {
        return("Dimensions of all matrices must be equal")
      }
      if (any(colnames(mat_ii) != colnames(mat_ii_1))) {
        return("Column names in all matrices must be equal")
      }
      if (any(rownames(mat_ii) != rownames(mat_ii_1))) {
        return("Row names in all matrices must be equal")
      }
    }
  }
  TRUE
})

# Methods --------------------------------------------------------------------------------

setGeneric("name_labs", function(x) standardGeneric("name_labs"))
#' Get the names of the matrices in a `ListMatrix` with label attributes
#'
#' @param x A `ListMatrix` object
#' @return A character vector with the names of the matrices. If the matrices have label
#'   attributes, the labels are returned as the names of the returned character vector.
#'
#' @export
setMethod("name_labs", signature(x = "ListMatrix"), function(x) {
  nms <- names(x)
  labs <- sapply(nms, \(.x) {
    l <- labelled::label_attribute(x[[.x]])
    if (is.null(l)) .x else l
  })
  stats::setNames(nms, nm = labs)
})


#' @param x A `ListMatrix` object
#' @rdname ListMatrix-class
#' @export
setMethod("as.list", signature(x = "ListMatrix"), function(x) {
  stats::setNames(x@.Data, nm = names(x))
})

#' @rdname ListMatrix-class
#' @export
setMethod("dim", signature(x = "ListMatrix"), function(x) {
  if (length(x) == 0) return(c(0, 0))
  dim(x[[1]])
})
#' @rdname ListMatrix-class
setGeneric("nrow", function(x) standardGeneric("nrow"))
#' @rdname ListMatrix-class
#' @export
setMethod("nrow", signature(x = "ListMatrix"), function(x) dim(x)[1])
#' @rdname ListMatrix-class
setGeneric("ncol", function(x) standardGeneric("ncol"))
#' @rdname ListMatrix-class
#' @export
setMethod("ncol", signature(x = "ListMatrix"), function(x) dim(x)[2])

# setGeneric("dimnames", function(x) standardGeneric("dimnames"))    # Primitive
#' @rdname ListMatrix-class
#' @export
setMethod("dimnames", signature(x = "ListMatrix"), function(x) {
  if (length(x) == 0) return(list(NULL, NULL))
  dimnames(x[[1]])
})
#' @rdname ListMatrix-class
#' @export
setMethod("dimnames<-", signature(x = "ListMatrix"), function(x, value) {
  for(ii in seq_along(x)) {
    dimnames(x[[ii]]) <- value
  }
  x
})
setGeneric("colnames", function(x, do.NULL = TRUE, prefix = "col") standardGeneric("colnames"))
#' @rdname ListMatrix-class
#' @export
setMethod("colnames", signature(x = "ListMatrix"), function(x) dimnames(x)[[2]])
setGeneric("colnames<-", function(x, value) standardGeneric("colnames<-"))
#' @rdname ListMatrix-class
#' @export
setMethod("colnames<-", signature(x = "ListMatrix"), function(x, value) {
  for(ii in seq_along(x)) {
    colnames(x[[ii]]) <- value
  }
  x
})
setGeneric("rownames", function(x, do.NULL = TRUE, prefix = "row") standardGeneric("rownames"))
#' @rdname ListMatrix-class
#' @export
setMethod("rownames", signature(x = "ListMatrix"), function(x) dimnames(x)[[1]])
setGeneric("rownames<-", function(x, value) standardGeneric("rownames<-"))
#' @rdname ListMatrix-class
#' @export
setMethod("rownames<-", signature(x = "ListMatrix"), function(x, value) {
  for(ii in seq_along(x)) {
    rownames(x[[ii]]) <- value
  }
  x
})

#' @rdname ListMatrix-class
#' @export
setMethod("show", signature(object = "ListMatrix"), function(object) {
  d <- dim(object)
  cat("ListMatrix with", length(object), "matrices with dimension of", d)
  for(ii in seq_along(object)) {
    cat("\nMatrix", ii, ":\n")
    print(object[[ii]][1:min(d[1L], 5), 1:min(d[2L], 5)])
  }
})
#' @rdname ListMatrix-class
#' @export
setMethod("print", signature(x = "ListMatrix"), function(x) show(x))




