#' A class for storing a list of matrices
#'
#' @slot matrices A list with matrices. All matrices must have the same dimensions and
#'   row/column names.
#' @exportClass ListMatrix
#' @export
ListMatrix <- setClass(
  "ListMatrix",
  slots = c(matrices = "list")
)

setMethod(
  "initialize", "ListMatrix",
  function(.Object, ...) {
    .Object@matrices <- list(...)
    validObject(.Object)
    return(.Object)
  }
)

setValidity("ListMatrix", function(object) {
  for(ii in seq_along(object@matrices)) {
    mat_ii <- object@matrices[[ii]]
    if (!is.matrix(mat_ii)) return("All elements of @matrices must be matrices")
    if (ncol(mat_ii) > 0 & is.null(colnames(mat_ii))) {
      return("Column names in all matrices must be defined")
    }
    if (nrow(mat_ii) > 0 & is.null(rownames(mat_ii))) {
      return("Row names in all matrices must be defined")
    }
    if (ii > 1) {
      mat_ii_1 <- object@matrices[[ii - 1]]
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

#' Get the names of the matrices in a `ListMatrix`
#'
#' @param x A `ListMatrix` object
#' @return `names` returns a character vector with the names of the matrices.
#'    `name_labs` returns label attributes are attached as the names of the returned names, if
#'    the matrices have label attributes by `labelled`,
#' @name names
#' @export
setMethod("names", signature(x = "ListMatrix"), function(x) names(x@matrices))
setGeneric("name_labs", function(x) standardGeneric("name_labs"))
#' @rdname names
#' @export
setMethod("name_labs", signature(x = "ListMatrix"), function(x) {
  nms <- names(x@matrices)
  labs <- sapply(nms, \(.x) {
    l <- labelled::label_attribute(x@matrices[[.x]])
    if (is.null(l)) .x else l
  })
  stats::setNames(nms, nm = labs)
})


#' Extract or Replace an element of a `ListMatrix`
#'
#' @param x A `ListMatrix` object
#' @param i An index or a name of the element to extract/replace
#' @name Extract
NULL
#' @rdname Extract
#' @export
setMethod("[[", signature(x = "ListMatrix"), function(x, i, ...) x@matrices[[i]])
#' @rdname Extract
#' @export
setMethod("[[<-", signature(x = "ListMatrix"), function(x, i, ..., value) {
  x@matrices[[i]] <- value
  x
})


#' @param x A `ListMatrix` object
#' @rdname ListMatrix-class
#' @export
setMethod("as.list", signature(x = "ListMatrix"), function(x) x@matrices)

#' @rdname ListMatrix-class
#' @export
setMethod(
  "dim",
  signature(x = "ListMatrix"),
  function(x) {
    if (length(x@matrices) == 0) return(c(0, 0))
    dim(x@matrices[[1]])
  }
)
#' @rdname ListMatrix-class
setGeneric("nrow", function(x) standardGeneric("nrow"))
#' @rdname ListMatrix-class
#' @export
setMethod("nrow", signature(x = "ListMatrix"), function(x) dim(x)[1L])
#' @rdname ListMatrix-class
setGeneric("ncol", function(x) standardGeneric("ncol"))
#' @rdname ListMatrix-class
#' @export
setMethod("ncol", signature(x = "ListMatrix"), function(x) dim(x)[2L])

# setGeneric("dimnames", function(x) standardGeneric("dimnames"))    # Primitive
#' @rdname ListMatrix-class
#' @export
setMethod("dimnames", signature(x = "ListMatrix"), function(x) {
  if (length(x@matrices) == 0) return(list(NULL, NULL))
  dimnames(x@matrices[[1]])
})
#' @rdname ListMatrix-class
#' @export
setMethod("dimnames<-", signature(x = "ListMatrix"), function(x, value) {
  for(ii in seq_along(x@matrices)) {
    dimnames(x@matrices[[ii]]) <- value
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
  for(ii in seq_along(x@matrices)) {
    colnames(x@matrices[[ii]]) <- value
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
  for(ii in seq_along(x@matrices)) {
    rownames(x@matrices[[ii]]) <- value
  }
  x
})

#' @rdname ListMatrix-class
#' @export
setMethod("show", signature(object = "ListMatrix"), function(object) {
  cat("ListMatrix with", length(object@matrices), "matrices\n")
  for(ii in seq_along(object@matrices)) {
    d <- dim(object@matrices[[ii]])
    cat("\nMatrix", ii, ":", d, "\n")
    print(object@matrices[[ii]][1:min(d[1L], 5), 1:min(d[2L], 5)])
  }
})
#' @rdname ListMatrix-class
#' @export
setMethod("print", signature(x = "ListMatrix"), function(x) show(x))




