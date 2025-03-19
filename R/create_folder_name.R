

#' Create a Clean Folder Name
#'
#' This function takes a text string and returns a clean, standardized name for a folder.
#'
#' @param x A character string to be converted into a folder-friendly name.
#' @param sep A character used as the separator between words (default is "-").
#'
#' @return A character string formatted as a clean folder name.
#' @export
#'
#' @importFrom janitor make_clean_names
#' @importFrom stringr str_replace_all
#'
#' @examples
#' create_folder_name("My Folder Name") # Returns "my-folder-name"
#' create_folder_name("Another_example", sep = "_") # Returns "another_example"
create_folder_name <- function(x, sep = "-") {
  x <- janitor::make_clean_names(x)
  x <- stringr::str_replace_all(x, "_", sep)
  return(x)
}
