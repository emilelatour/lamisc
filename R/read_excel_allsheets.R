
#' @title
#' Read in all sheets in an excel spreadsheet.
#'
#' @description
#' Helper function to quickly and easily read in all the sheets in an excel spreadsheet using `readxl::read_excel`
#'
#' @param file Path to the xls/xlsx file
#' @param tibble `TRUE` to return a tibble, `FALSE` to return data frame.
#' @param ... Additional arguments passes to `readxl::read_excel`
#'
#' @importFrom readxl excel_sheets
#' @importFrom readxl read_excel
#'
#' @return
#' A list of tibbles.
#'
#' @export
#'
#' @examples
#' library(readxl)
#' datasets <- readxl_example("datasets.xlsx")
#' read_excel_allsheets(datasets)

read_excel_allsheets <- function(file, tibble = TRUE, ...) {

  sheets <- readxl::excel_sheets(file)

  x <- lapply(sheets, function(x) readxl::read_excel(file,
                                                     sheet = x, ...))

  if(!tibble) {
    x <- lapply(x, as.data.frame)
  }

  names(x) <- sheets

  return(x)
}
