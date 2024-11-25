#' A class for storing a list of matrices
#'
#' @slot matrices A list with the matrices
#' @exportClass ListMatrix
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
    if (!is.matrix(object@matrices[[ii]])) return("All elements of @matrices must be matrices")
    if (ii > 1) {
      mat_ii <- object@matrices[[ii]]
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

#' @param x A `ListMatrix` object
#'
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

#' @rdname ListMatrix-class
#' @export
setMethod("names", signature(x = "ListMatrix"), function(x) names(x@matrices))
#' @rdname ListMatrix-class
#' @export
setMethod("[[", signature(x = "ListMatrix", i = "ANY"), function(x, i) x@matrices[[i]])
#' @rdname ListMatrix-class
#' @export
setMethod("[[<-", signature(x = "ListMatrix", i = "ANY"), function(x, i, value) {
  x@matrices[[i]] <- value
  x
})




