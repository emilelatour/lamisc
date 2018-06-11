#' @title
#' Take a list of data frames and write them to individual data frames with
#' names
#'
#' @description
#' The function will take a list of data frames in the environment and extract
#' each data frame and assign the name from the list.
#'
#' @references
#' \url{https://stackoverflow.com/questions/9726705/assign-multiple-objects-to-globalenv-from-within-a-function}
#'
#' @param list_of_dfs A list with data frames
#' @param envir Environment to assign data frames to. Default is `.GlobalEnv`.
#'
#' @return Data frames; the same number as were contained in the list. Returned
#'   to the global environment.
#' @export
#'
#' @examples
#' library(readxl)
#' path <- readxl::readxl_example("datasets.xlsx")
#' list_of_sheets <- lamisc::read_excel_all_sheets(path_to_excel_file = path,
#'                                                 as_list = TRUE)
#' extract_df_from_list(list_of_sheets)
#'
extract_df_from_list <- function(list_of_dfs, envir = .GlobalEnv) {

  for (i in 1:length(list_of_dfs)) {
    assign(names(list_of_dfs[i]),
           list_of_dfs[[i]],
           envir = envir)

  }
}
