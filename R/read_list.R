#' @title
#' Read a lot of datasets at once with R
#'
#' @description
#' Supply a list of data sets and the function to read the datasets. For
#' example, to read in `.csv` files, use `readr::read_csv`. This function comes
#' from Bruno Rodrigues' website and credit goes to him.
#'
#' If you prefer not to have the datasets in a list, but rather import them into
#' the global environment, then use `read_list2`.
#'
#' @references
#' \url{http://www.brodrigues.co/blog/2016-07-26-read-a-lot-of-datasets-at-once-with-r/}
#'
#' @name read_list
#'
#' @param list_of_datasets A character vector with names of data sets to read.
#' @param read_func Function to read the data sets.
#' @param envir Environment to assign data frames to. Default is `.GlobalEnv`.
#'   Used in `read_list2` only.
#'
#' @return A list of data sets
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
#' #### read_list --------------------------------
#' list_of_data_sets <- read_list(list_of_datasets = data_files,
#'                                read_func = readr::read_csv)
#'
#' dplyr::glimpse(list_of_data_sets)
#'
#'
#' #### read_list2 --------------------------------
#' read_list2(list_of_datasets = data_files,
#'            read_func = readr::read_csv)
#' }

read_list <- function(list_of_datasets, read_func){

  read_and_assign <- function(dataset, read_func){
    dataset_name <- as.name(dataset)
    dataset_name <- read_func(dataset)
  }

  # invisible is used to suppress the unneeded output
  output <- invisible(
    sapply(list_of_datasets,
           read_and_assign, read_func = read_func,
           simplify = FALSE, USE.NAMES = TRUE))

  # Remove the extension at the end of the data set names
  names_of_datasets <- c(unlist(strsplit(list_of_datasets, "[.]"))[c(T, F)])
  names(output) <- names_of_datasets
  return(output)
}

#' @rdname read_list
#' @export
read_list2 <- function(list_of_datasets, read_func, envir = .GlobalEnv) {

  read_and_assign <- function(dataset, read_func) {
    assign(dataset, read_func(dataset), envir = envir)
  }

  # invisible is used to suppress the unneeded output
  output <- invisible(
    sapply(list_of_datasets,
           read_and_assign, read_func = read_func,
           simplify = FALSE, USE.NAMES = TRUE))

}
