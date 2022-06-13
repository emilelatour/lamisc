#' @title
#' Create folder with structure for analysis
#'
#' @description
#' A helper function that will create folders for an analysis project in the
#' directory that is specified. Note that established folders should not be
#' overwritten since this funcion relies on `fs::dir_create()` but do use
#' caution.
#'
#' @param main_dir File path to the main directory where you want to create
#'   folders. Default is the current working directory (`getwd()`)
#' @param sub_dir Name of the sub-directory where you want to create folders.
#'   Default is `Null`; folder can be completely specified using `main_dir` if
#'   you'd like.
#' @param is_iit Default is `FALSE` and the default folder names below are
#'     used. If `TRUE` then the following BSR IIT folders are used `c("Admin",
#'     "Data", "Code", "Protocol", "EDC", "Deliverables", "Publishing")`
#' @param folder_names A character vector of the folder names to create in the
#'   directory that you specified. Defaults are `c("data", "code", "docs",
#'   "admin", "deliverables")`.
#'
#' @importFrom fs path_tidy
#' @importFrom fs dir_exists
#' @importFrom fs dir_create
#' @importFrom glue glue
#'
#' @export
#'
#' @examples \dontrun{
#' create_proj_folders(main_dir = "/Users/latour/Desktop/",
#'                     sub_dir = "new_folder")
#' }
create_proj_folders <- function(main_dir = getwd(),
                                sub_dir = NULL,
                                is_iit = FALSE,
                                folder_names = c("data",
                                                 "code",
                                                 "docs",
                                                 "admin",
                                                 "deliverables")) {

  main_dir <- fs::path_tidy(main_dir)

  if (!is.null(sub_dir)) {
    sub_dir <- fs::path_tidy(sub_dir)
  } else {
    sub_dir <- fs::path_tidy("")
  }


  file_path <- glue::glue("{main_dir}/{sub_dir}")
  file_path <- fs::path_tidy(file_path)

  if (!fs::dir_exists(file_path)) {
    fs::dir_create(file_path)

  }


  if (is_iit) {
    folder_names <- c("Admin",    # Bridge support request emails, contract/budget info
                      "Data",     # EDC test (UAT) and real trial participant data
                      "Code",
                      "Protocol", # LOIs, CRRC/IRB docs, manufacturer docs, FDA docs
                      "EDC",      # Blueprints, CRF templates, Catherine’s Endpoint Collection Tool
                      "Deliverables",  # anything BSR sends out to collaborators (eg, sample size statement; annotated Blueprint)
                      "Publishing")   # abstracts, posters, manuscripts
  }


  glue::glue("{file_path}/{folder_names}") %>%
    fs::path_tidy(.) %>%
    fs::dir_create(.)

}
