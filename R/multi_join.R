#' @title
#' Merge a list of datasets together
#'
#' @description
#' Once again, a wonderful function provided by Bruno Rodrigues. This time a
#' very simple function that uses this list of read datasets and merges them all
#' together.
#'
#' You should make sure that all the data frames have the same column names but
#' you can also join data frames with different column names if you give the
#' argument `by` to the join function. This is possible thanks to `...` that
#' allows you to pass further argument to `join_func()`.
#'
#'
#'
#' @param list_of_loaded_data A list of data sets.
#' @param join_func A user supplied join or merge function.
#' @param ... Further argument to `join_func()`.
#'
#' @return A merged tibble / data frame
#' @export
#'
#' @examples \dontrun{
#' library(dplyr)
#' library(stringr)
#' library(readr)
#'
#' csv_path <- "C:/Users/latour/Dropbox/repos/lamisc/inst/extdata"
#'
#' (data_files <- list.files(path = csv_path,
#'                           pattern = ".csv"))
#'
#' setwd(csv_path)
#'
#' list_of_data_sets <- read_list(list_of_datasets = data_files,
#'                                read_func = readr::read_csv)
#'
#' dplyr::glimpse(list_of_data_sets)
#' merged_data <- multi_join(list_of_loaded_data = list_of_data_sets,
#'                           join_func = dplyr::full_join)
#' class(merged_data)
#' dplyr::glimpse(merged_data)
#' }


multi_join <- function(list_of_loaded_data, join_func, ...) {

  output <- Reduce(function(x, y) {join_func(x, y, ...)}, list_of_loaded_data)

  return(output)
}


