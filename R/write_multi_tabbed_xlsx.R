

#' @title
#' Easily Make Multi-tabbed .xlsx Files with openxlsx
#'
#' @description
#' Wrapper for some steps using `openxlsx` package to write multiple to multiple
#' tabs in an Excel spreadsheet. Apparently, `openxlsx::write.xlsx()` will
#' accomplish the same thing. I just found this out. I use this other method so
#' regularly that I still feel like saving it here for posterity.
#'
#' @references
#' \itemize{
#' \item \href{https://trinkerrstuff.wordpress.com/2018/02/14/easily-make-multi-tabbed-xlsx-files-with-openxlsx/}{TRinker's R Blog}
#'
#' }
#'
#' @param tab_names Character vector of names for the tabs in the Excel
#'   spreadsheet.
#' @param list_of_dfs A list of data frames to write to file.
#' @param file_path Path and filename to write to.
#' @param over_write Logical. Overwrite the existing file of the same name.
#'   Default is `TRUE`
#'
#' @importFrom openxlsx createWorkbook
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeData
#' @importFrom openxlsx saveWorkbook
#' @importFrom purrr walk2
#'
#' @return
#' A workbook object
#'
#' @export
#'
#' @examples \dontrun{
#' dat <- split(mtcars, mtcars$cyl)
#' dat
#'
#' write_multi_tabbed_xlsx(tab_names = c("4 cyl", "6 cyl", "8 cyl"),
#'                         list_of_dfs = dat,
#'                         file_path = "C:/Users/latour/Desktop/cylinders.xlsx",
#'                         over_write = TRUE)
#' }

write_multi_tabbed_xlsx <- function(tab_names,
                                    list_of_dfs,
                                    file_path,
                                    over_write = TRUE) {

  # Create a blank workbook
  wb <- openxlsx::createWorkbook()

  # Write to the workbook
  purrr::walk2(.x = tab_names,
               .y = list_of_dfs,
               .f = ~ write_sheets(wb = wb,
                                   sheet = .x,
                                   data = .y))

  # Save the workbook
  openxlsx::saveWorkbook(wb,
                         file = file_path,
                         overwrite = over_write)


}


#### Helper function for write_multi_tabbed_xlsx ------------------------------
# Function to write to file
write_sheets <- function(wb, sheet, data) {
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, data, rowNames = FALSE)}
