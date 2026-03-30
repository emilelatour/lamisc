#' Create a new Quarto document from the lamisc template
#'
#' Copies the lamisc skeleton `.qmd` into the current working directory (or a
#' named sub-directory) and opens it for editing. Mirrors the pattern used by
#' `rmarkdown::draft()` but for Quarto.
#'
#' @param file_name Character string. Name for the new `.qmd` file (without
#'   extension). If `subdir = TRUE` a sub-directory with this name is also
#'   created and the file is placed inside it. Defaults to `"analysis"`.
#' @param subdir Logical. If `TRUE` (default) a sub-directory named
#'   `file_name` is created and the file is written there. Set to `FALSE` to
#'   write directly into the current working directory.
#' @param open Logical. If `TRUE` (default) the new file is opened in the
#'   RStudio editor via [utils::file.edit()].
#'
#' @return The path to the new `.qmd` file, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Creates ./my-analysis/my-analysis.qmd and opens it
#' use_quarto_template("my-analysis")
#'
#' # Creates ./report.qmd in the current directory without opening
#' use_quarto_template("report", subdir = FALSE, open = FALSE)
#' }
use_quarto_template <- function(file_name = "analysis",
                                subdir    = TRUE,
                                open      = TRUE) {

  stopifnot(
    "`file_name` must be a single non-empty character string" =
      is.character(file_name) && length(file_name) == 1L && nchar(file_name) > 0
  )

  # ---- Resolve output path --------------------------------------------------

  if (subdir) {
    out_dir <- file_name
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
      message("Created sub-directory: '", out_dir, "'")
    }
  } else {
    out_dir <- "."
  }

  out_file <- file.path(out_dir, paste0(file_name, ".qmd"))

  if (file.exists(out_file)) {
    stop(
      "File already exists: '", out_file, "'. ",
      "Choose a different `file_name` or delete the existing file."
    )
  }

  # ---- Locate skeleton inside lamisc ----------------------------------------

  skeleton_path <- system.file(
    "extdata/_extensions/lamisc/skeleton.qmd",
    package = "lamisc"
  )

  if (!nzchar(skeleton_path)) {
    stop(
      "Could not find the lamisc skeleton template. ",
      "Try re-installing lamisc with `devtools::install_github('emilelatour/lamisc')`."
    )
  }

  # ---- Copy skeleton to destination -----------------------------------------

  file.copy(from = skeleton_path, to = out_file)
  message("Created new Quarto document: '", out_file, "'")

  # ---- Open in editor -------------------------------------------------------

  if (open && rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(out_file)
  } else if (open) {
    utils::file.edit(out_file)
  }

  invisible(out_file)
}
