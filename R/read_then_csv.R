#' @name read_then_csv
#'
#' @title
#' CSV caching and iterating over sheers
#'
#' @description
#' Read in all the sheets from an excel file at once and simultaneously cache to
#' CSV
#'
#' @references
#' \url{https://readxl.tidyverse.org/articles/articles/readxl-workflows.html#csv-caching-and-iterating-over-sheets}
#'
#' @import dplyr
#' @importFrom rlang set_names
#' @importFrom tools file_path_sans_ext
#' @importFrom readxl read_excel
#' @importFrom readr write_csv
#' @importFrom purrr map
#'
#' @param sheet String; Name of an excel sheet
#' @param path String; filepath to the the excel file
#'
#' @return NULL. CSV files are written to the same location as the excel file
#'
#' @rdname read_then_csv
#' @export
#'
#' @examples
#' library(dplyr)
#' library(readxl)
#' library(rlang)
#' library(purrr)
#'
#' path <- readxl::readxl_example("datasets.xlsx")
#' path %>%
#'   readxl::excel_sheets() %>%
#'   rlang::set_names() %>%
#'   purrr::map(read_then_csv, path = path)
#'
#' read_then_csv2(path)
read_then_csv <- function(sheet, path) {

  pathbase <- path %>%
    base::basename() %>%
    tools::file_path_sans_ext()

  path %>%
    readxl::read_excel(sheet = sheet) %>%
    readr::write_csv(paste0(pathbase, "-", sheet, ".csv"))
}


#' @rdname read_then_csv
#' @export
read_then_csv2 <- function(path) {

  path %>%
    readxl::excel_sheets() %>%
    rlang::set_names() %>%
    purrr::map(.x = .,
               .f = ~ lamisc::read_then_csv(sheet = .x, path = path))

}
