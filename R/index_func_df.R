#' @title
#' Get the occurence of duplicate combinations of variables in a data frame.
#'
#' @description
#' Similar to my other function `index_func`, this one is more appropriate if
#' you are checking for duplciate values across more than one column.
#'
#' @import dplyr
#' @import rlang
#'
#' @param data A data frame or tibble
#' @param ... Unquoted variable names to search for duplicates.
#'
#' @return
#' Returns a data.frame (actually a \code{tbl_df}) with the full records and a
#' variable \code{dupe_count} showing the number of rows sharing a combination
#' of the specified variables have duplicated values duplicated values; shows
#' the increasing number, i.e. the occurence.
#'
#' @export
#'
#' @examples
#' library(tibble)
#' bar <- tibble::tribble(
#'   ~var1, ~var2, ~var3,
#'      4L,    4L,    1L,
#'     23L,   23L,    3L,
#'      4L,    4L,    3L,
#'     23L,   23L,    2L,
#'      5L,    5L,    1L,
#'     43L,   43L,    3L,
#'     54L,   54L,    1L,
#'     56L,   56L,    2L,
#'    657L,  657L,    2L,
#'     67L,   67L,    1L,
#'     67L,   67L,    2L,
#'    435L,  435L,    1L,
#'    453L,  453L,    2L,
#'    435L,  435L,    2L,
#'    324L,  324L,    3L,
#'     34L,   34L,    1L,
#'    456L,  456L,    1L,
#'     56L,   56L,    1L,
#'    567L,  567L,    3L,
#'     65L,   65L,    4L,
#'     34L,   34L,    2L,
#'    435L,  435L,    3L
#'   )
#'
#' index_func_df(bar)
#' index_func_df(bar, var1)
#' index_func_df(bar, var1, var3)

index_func_df <- function(data, ...) {

  # unquoted names for NSE calls, need quoted names separately for messages +
  # warnings
  uq_names <- as.list(substitute(list(...)))[-1L]
  df_name <- deparse(substitute(data))

  # if called on an entire data.frame with no specified variable names
  if (length(uq_names) == 0) {
    var_names <- names(data)
    vars <- rlang::syms(var_names)
    message("No variable names specified - using all columns.\n")
  } else {
    vars <- rlang::enquos(...)
  }

  data %>%
      dplyr::group_by(!!! vars) %>%
      dplyr::mutate(dupe_count = sequence(dplyr::n()))

}


