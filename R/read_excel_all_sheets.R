#' @title
#' Read in all the sheets at once from an excel file
#'
#' @description
#' Given an excel file with muliple sheets, the function will iterate over each
#' sheet and save them all to a list or to individual data frames.
#'
#' @references
#' \url{https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames}
#'
#' \url{https://readxl.tidyverse.org/articles/articles/readxl-workflows.html#iterate-over-multiple-worksheets-in-a-workbook}
#'
#' @import dplyr
#' @import readxl
#' @importFrom purrr set_names
#' @importFrom purrr map
#'
#' @param path_to_excel_file File path to the Excel file with multiple sheets
#' @param as_list Logical; if `TRUE` Returns a list of data frames. If `FALSE`,
#'   multiple data frames are returned to the global environment.
#'
#' @return A list of data frames. Or a number of data frames.
#' @export
#'
#' @examples
#' library(readxl)
#' path <- readxl::readxl_example("datasets.xlsx")
#'
#' #### As a list --------------------------------
#'
#' list_of_sheets <- read_excel_all_sheets(path_to_excel_file = path,
#'                                         as_list = TRUE)
#' list_of_sheets
#'
#' #### As data frames --------------------------------
#'
#' read_excel_all_sheets(path_to_excel_file = path,
#'                       as_list = FALSE)
read_excel_all_sheets <- function(path_to_excel_file, as_list = TRUE) {

  #### old way --------------------------------

  # sheet_names <- readxl::excel_sheets(path_to_excel_file)
  #
  # sheets_as_list <- lapply(sheets,
  #                          function(x) readxl::read_excel(path_to_excel_file,
  #                                                         sheet = x))
  #
  # names(sheets_as_list) <- sheet_names


  #### Newer way with purrr --------------------------------

  sheets_as_list <- readxl::excel_sheets(path_to_excel_file) %>%
    purrr::set_names(.) %>%
    purrr::map(.x = .,
               .f = ~ readxl::read_excel(path_to_excel_file, sheet = .x))



  #### Return a list or individual data frames --------------------------------

  if (as_list == TRUE) {

    sheets_as_list

  } else {

    lamisc::extract_df_from_list(sheets_as_list)

  }


}
