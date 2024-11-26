
setGeneric("as_tibble", function(x, ...) standardGeneric("as_tibble"))
#' Coerce a SumExp object to a `tibble`
#'
#' @param x A `SumExp` object
#' @return A `tibble` with the row and column data frames as well as the matrices. Each matrix
#'   in the `SumExp` object is represented as a long format tibble with columns `.row_id`,
#'   `.col_id`, and the matrix name.
#' @export
setMethod(
  "as_tibble", signature(x = "SumExp"),
  function(x, ...) {
    col_tbl <- tibble::as_tibble(x@col_df, rownames = ".col_id")
    row_tbl <- tibble::as_tibble(x@row_df, rownames = ".row_id")

    stopifnot("Names of @matrices must be unique" = anyDuplicated(names(x@matrices)) == 0)
    mx <- lapply(names(x@matrices), \(ii) {
      .x <- x@matrices[[ii]]
      out <- tibble::as_tibble(.x, rownames = ".row_id") |>
        tidyr::pivot_longer(cols = -c(.row_id), names_to = ".col_id", values_to = ii)
    })
    mx <- purrr::reduce(mx, dplyr::full_join, by = c(".row_id", ".col_id"))
    # Copy the label attributes
    for(ii in names(mx)[names(mx) %in% names(x@matrices)]) {
      labelled::label_attribute(mx[[ii]]) <- labelled::label_attribute(x@matrices[[ii]])
    }

    mx |>
      dplyr::left_join(row_tbl, by = ".row_id") |>
      dplyr::left_join(col_tbl, by = ".col_id")
  }
)

setGeneric("ggplot",
           function(data, mapping = aes(), ..., environment = parent.frame()) {
             standardGeneric("ggplot")
           })
#' Create a ggplot object from a SumExp object
#'
#' @param data A `SumExp` object
#' @inheritParams ggplot2::ggplot
#' @return A ggplot object
#' @export
setMethod(
  "ggplot", signature(data = "SumExp"),
  function(data, mapping = aes(), ..., environment = parent.frame()) {
    data |>
      as_tibble() |>
      ggplot2::ggplot(mapping = mapping, ..., environment = environment)
  }
)
