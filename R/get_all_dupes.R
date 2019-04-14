
#' @title
#' Find all duplicate rows
#'
#' @description
#' Identifies the duplicate values in a column of a data frame (or tibble) and
#' returns a data frame (tibble) with just the duplicate rows.
#'
#' I actually prefer \code{janitor::get_dupes()} but I am including this version
#' for posterity.
#'
#' @references
#'
#' \url{https://stat.ethz.ch/pipermail/r-help/2011-October/291383.html}
#'
#' \url{https://stackoverflow.com/questions/7854433/finding-all-duplicate-rows-including-elements-with-smaller-subscripts}
#'
#' @param data A data frame or tibble
#' @param x The unquoted column name to check for duplicate values
#'
#' @return A tibble similar to data
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' vec <- tibble::tribble(
#'   ~ID,      ~OS,         ~Date,
#'   "userA",    "Win",  "12/15/2015",
#'   "userB",    "OSX",    "1/1/2016",
#'   "userA",    "Win",  "12/15/2017",
#'   "userC",  "Win64",    "4/9/2018"
#' )
#'
#' all_dupes(data = vec, x = ID)
#' vec %>%
#'   all_dupes(ID)

all_dupes <- function(data, x) {

  x <- deparse(substitute(x))

  data[duplicated(data[[x]]) | duplicated(data[[x]], fromLast = TRUE), ]

}



#### Old version --------------------------------
#
# allDup <- function(value) {
#   duplicated(value) | duplicated(value, fromLast = TRUE)
# }
#
#
# ## Example of use ---------------
#
# vec <- tibble::tribble(
#   ~ID,      ~OS,         ~Date,
#   "userA",    "Win",  "12/15/2015",
#   "userB",    "OSX",    "1/1/2016",
#   "userA",    "Win",  "12/15/2017",
#   "userC",  "Win64",    "4/9/2018"
# )
#
# vec[allDup(vec$ID), ]
# # # A tibble: 2 x 3
# #   ID    OS    Date
# #   <chr> <chr> <chr>
# # 1 userA Win   12/15/2015
# # 2 userA Win   12/15/2017
