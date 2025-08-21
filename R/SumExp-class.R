#' @include ListMatrix-class.R
#' @include list-method.R
NULL

#' An light S4 class for the matrices with additional information for columns and rows
#'
#' @slot .Data A list with matrices, [`ListMatrix`]. All matrices must have the same dimensions
#'   and row/column names.
#' @slot col_df A data frame with additional information for columns
#' @slot row_df A data frame with additional information for rows
#' @slot metadata A [`list`] with any other additional information
#'
#' @param x,object A [`SumExp`] object
#' @examples
#' nms <- list(LETTERS[1:4], letters[1:5])
#' m1 <- matrix(sample(20), nrow = 4, dimnames = nms)
#' m2 <- matrix(sample(LETTERS, 20), nrow = 4, dimnames = nms)
#' df_c <- data.frame(
#'   x = c("alpha", "beta", "gamma", "delta", "epsilon"),
#'   type = gl(2, 3, 5, c("", "fruit")),
#'   row.names = nms[[2]]
#' )
#' df_r <- data.frame(
#'   y = rnorm(4),
#'   grp = rep(c("White", "Black"), each = 2),
#'   row.names = nms[[1]]
#' )
#' se <- SumExp(a = m1, b = m2, row_df = df_r, col_df = df_c)
#'
#' @importFrom methods setClass setMethod setValidity setGeneric callNextMethod validObject
#'   signature new show
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
           ...,
           col_df = data.frame(),
           row_df = data.frame(),
           metadata = list()) {
    .Object@col_df <- col_df
    .Object@row_df <- row_df
    .Object@metadata <- metadata
    callNextMethod(.Object, ...)
  }
)

.valid_SumExp_ListMatrix <- function(x) {
  if (ncol(x) != nrow(x@col_df)) {
    return("Number of columns in matrices must be equal to number of rows of @col_df")
  }
  if (any(colnames(x) != rownames(x@col_df))) {
    return("Column names in matrices must be equal to row names of @col_df")
  }
  if (nrow(x) != nrow(x@row_df)) {
    return("Number of rows in matrices must be equal to number of rows of @row_df")
  }
  if (any(rownames(x) != rownames(x@row_df))) {
    return("Row names in matrices must be equal to row names of @row_df")
  }
  NULL
}

.valid_SumExp_col_df <- function(x) {
  if (!is.data.frame(x@col_df)) {
    return("@col_df must be a data frame")
  }
  NULL
}

.valid_SumExp_row_df <- function(x) {
  if (!is.data.frame(x@row_df)) {
    return("@row_df must be a data frame")
  }
  NULL
}

setValidity("SumExp", function(object) {
  er_m <- c(
    .valid_SumExp_ListMatrix(object),
    .valid_SumExp_row_df(object),
    .valid_SumExp_col_df(object)
  )
  if (!is.null(er_m)) {
    return(er_m)
  }
  TRUE
})

# Methods --------------------------------------------------------------------------------


#' Accessors for [`SumExp`] objects
#'
#' @param value A data frame for `row_df<-` and `col_df<-` or a list for `metadata<-`
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
setMethod("colnames<-", signature(x = "SumExp"), function(x, value) {
  l <- as(x, "ListMatrix")    # Convert to ListMatrix
  colnames(l) <- value
  x@.Data <- l
  rownames(x@col_df) <- value
  x
})

#' @rdname SumExp-class
#' @export
setMethod("rownames<-", signature(x = "SumExp"), function(x, value) {
  l <- as(x, "ListMatrix")    # Convert to ListMatrix
  rownames(l) <- value
  x@.Data <- l
  rownames(x@row_df) <- value
  x
})


#' Print a data frame like printing a tibble
#'
#' @param x A data frame
#' @param ...,n Additional arguments passed to [tibble::print.tbl()]
#'
#' @returns The input data frame
#' @examples
#' not_export <- function() {
#' .print_data_frame_like_tibble(iris)
#' }
.print_data_frame_like_tibble <- function(x, ..., n = 5) {
  stopifnot(is.data.frame(x))
  # Add row names
  max5rownm <- rownames(x)[0:min(nrow(x), n)]
  rownm <- paste0("`", max5rownm, "`", collapse = ", ")
  if (nrow(x) > n) rownm <- paste0(rownm, "...")
  prefix <- "# - Row names:"
  max_width <- getOption("width") - nchar(prefix) - 4    # 4 for `...` and one extra
  if (nchar(rownm) > max_width) {     # Truncate row names
    rownm <- substr(rownm, 1, max_width - 3)
    rownm <- paste0(rownm, "...")
  }
  rownm <- paste(prefix, rownm)

  txt <- utils::capture.output(print(tibble::as_tibble(x), ..., n = n))
  txt[1] <- sub("tibble", "data.frame", txt[1])
  txt <- c(txt[1], rownm, txt[2:length(txt)])
  cat(txt, sep = "\n")
  invisible(x)
}

#' @rdname SumExp-class
#' @export
setMethod("show", signature(object = "SumExp"), function(object) {

  selectMethod("show", "ListMatrix")(object)
  cat("\n@row_df:\n")
  .print_data_frame_like_tibble(object@row_df)
  cat("\n@col_df:\n")
  .print_data_frame_like_tibble(object@col_df)
})
#' @rdname SumExp-class
#' @export
setMethod("print", signature(x = "SumExp"), function(x) show(x))


# #' @rdname SumExp-class
# #' @export
# setGeneric("assay", function(x, i, ...) standardGeneric("assay"))
# #' @rdname SumExp-class
# #' @export
# setMethod("assay", signature(x = "SumExp"), function(x, i, ...) x[[i]])
# #' @rdname SumExp-class
# #' @export
# setGeneric("assay<-", function(x, i, ..., value) standardGeneric("assay<-"))
# #' @rdname SumExp-class
# #' @export
# setMethod("assay<-", signature(x = "SumExp", value = "matrix"), function(x, i, ..., value) {
#   stopifnot("`i` must have a single value" = length(i) == 1)
#   x[[i]] <- value
#   x
# })


#' @rdname SumExp-class
#' @export
setMethod("t", signature(x = "SumExp"), function(x) {
  data_lst <- lapply(x, \(.x) {
    t(.x) |>
      labelled::copy_labels_from(.x)     # Copy labels of matrices
  })
  # Swap row and column data frames
  row_df <- subset_copy_labels.data.frame(x@col_df)
  col_df <- subset_copy_labels.data.frame(x@row_df)
  metadata <- x@metadata
  do.call("new", c(
    data_lst,
    list(Class = "SumExp", col_df = col_df, row_df = row_df, metadata = metadata)
  ))
})
