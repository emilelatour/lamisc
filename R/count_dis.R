
#' @title
#' Count distinct/unique rows
#'
#' @description
#' This is a pretty simple wrapper around `dplyr::distinct()`. I tend to do this
#' pipe chain alot when working with data with multiple row per subjects. Making
#' this for my own convenience more than anything else.
#'
#' @param .data a tbl
#' @param ... Optional variables to use when determining uniqueness. If there
#'   are multiple rows for a given combination of inputs, only the first row
#'   will be preserved. If omitted, will use all variables.
#' @param .keep_all If `TRUE``, keep all variables in `.data`. If a combination
#'   of `...`` is not distinct, this keeps the first row of values.
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr count
#' @importFrom dplyr pull
#'
#' @return
#' A numeric value
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' df <- tibble::tibble(
#'   x = sample(10, 100, rep = TRUE),
#'   y = sample(10, 100, rep = TRUE)
#' )
#'
#' count_dis(df, x, y)
#' count_dis(df, x, y)

count_dis <- function(.data, ..., .keep_all = FALSE) {

  dplyr::distinct(.data, ..., .keep_all = .keep_all) %>%
    dplyr::count() %>%
    dplyr::pull()
}

