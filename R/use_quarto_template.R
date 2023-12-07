
#' @title Create a quarto template
#'
#' @description
#' I followed [this great
#' post](https://spencerschien.info/post/r_for_nonprofits/quarto_template/) to
#' translate R Markdown’s templating within R packages to the Quarto protocol. I
#' wanted to be able to easily create the skeleton for my preferred quarto
#' settings without copying and pasting them from a file over and over again.
#'
#' All of the extension stuff, I don' care about and it has been commented out
#'
#'
#' @param file_name name of the sub-directory and new qmd report within it
# #' @param ext_name name the specific extension
#'
#' @export
# use_quarto_ext <- function(file_name = NULL,
#                            ext_name = "quartotemplate") {

use_quarto_template <- function(file_name = NULL) {


  if (is.null(file_name)) {
    stop("You must provide a valid file_name")
  }

  out_dir <- file_name

  if(!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # # check for available extensions
  # stopifnot("Extension not in package" = ext_name %in% c("quartotemplate"))
  #
  # # check for existing _extensions directory
  # if(!file.exists("_extensions")) dir.create("_extensions")
  # message("Created '_extensions' folder")
  #
  # # various reading of key-value pairs for reporting
  # ext_yml <- readLines(system.file("extdata/_extensions/quartotemplate/_extension.yml",
  #                                  package = "quartotemplate"))
  #
  # ext_ver <- gsub(
  #   x = ext_yml[grepl(x = ext_yml, pattern = "version:")],
  #   pattern = "version: ",
  #   replacement = ""
  # )
  #
  # ext_nm <- gsub(
  #   x = ext_yml[grepl(x = ext_yml, pattern = "title:")],
  #   pattern = "title: ",
  #   replacement = ""
  # )
  #
  # # Create folder for recursive copying into ahead of time
  # if(!file.exists(paste0("_extensions/", ext_name))) dir.create(paste0("_extensions/", ext_name))
  #
  # # copy from internals
  # file.copy(
  #   from = system.file(paste0("extdata/_extensions/", ext_name), package = "quartotemplate"),
  #   to = paste0("_extensions/"),
  #   overwrite = TRUE,
  #   recursive = TRUE,
  #   copy.mode = TRUE
  # )
  #
  # # logic check to make sure extension files were moved
  # n_files <- length(dir(paste0("_extensions/", ext_name)))
  #
  # if(n_files >= 2){
  #   message(paste(ext_nm, "v", ext_ver, "was installed to _extensions folder in current working directory."))
  # } else {
  #   message("Extension appears not to have been created")
  # }

  # create new qmd report based on skeleton
  readLines("_extensions/quartotemplate/skeleton.qmd") |>
    writeLines(text = _,
               con = paste0(out_dir, "/", file_name, ".qmd", collapse = ""))

  # open the new file in the editor
  file.edit(paste0(out_dir, "/", file_name, ".qmd", collapse = ""))

  # # copy header.tex over as well
  # readLines("_extensions/quartotemplate/header.tex") |>
  #   writeLines(text = _, con = paste0(out_dir, "/header.tex"))

}
