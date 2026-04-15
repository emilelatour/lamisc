#' Create a new Quarto document from the lamisc skeleton template
#'
#' Copies the lamisc skeleton `.qmd` template into a specified location,
#' optionally creating a subdirectory to keep the file self-contained. The
#' new file is opened in the editor automatically.
#'
#' @param file_name A single non-empty character string giving the base name of
#'   the new Quarto document (without the `.qmd` extension). Also used as the
#'   subdirectory name when `subdir = TRUE`. Default is `"analysis"`.
#' @param path A single character string giving the parent directory in which to
#'   create the file (or subdirectory). Relative paths are resolved from the
#'   current working directory, which in an RStudio project is typically the
#'   project root. If the directory does not exist it will be created. Default
#'   is `"."` (the current working directory).
#' @param subdir Logical. If `TRUE`, a subdirectory named
#'   `file_name` is created inside `path` and the `.qmd` file is placed inside
#'   it. If `FALSE` (the default), the file is placed directly in `path`.
#' @param open Logical. If `TRUE` (the default), the newly created file is
#'   opened in the editor. Uses [rstudioapi::navigateToFile()] when RStudio is
#'   available, otherwise falls back to [utils::file.edit()].
#'
#' @return The path to the newly created `.qmd` file, invisibly.
#'
#' @details
#' The skeleton template is stored in the lamisc package at
#' `inst/extdata/_extensions/lamisc/skeleton.qmd` and is located at runtime
#' via [base::system.file()]. If the skeleton cannot be found, the function
#' will prompt you to re-install lamisc from GitHub.
#'
#' The `path` and `subdir` arguments interact as follows:
#'
#' | `path`   | `subdir` | Output location                              |
#' |----------|----------|----------------------------------------------|
#' | `"."`    | `TRUE`   | `./file_name/file_name.qmd`                  |
#' | `"."`    | `FALSE`  | `./file_name.qmd`                            |
#' | `"code"` | `TRUE`   | `code/file_name/file_name.qmd`               |
#' | `"code"` | `FALSE`  | `code/file_name.qmd`                         |
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create analysis.qmd in a new subfolder at the project root
#' use_quarto_template()
#'
#' # Create a named file directly in the project root (no subfolder)
#' use_quarto_template("01_data_cleaning", subdir = FALSE)
#'
#' # Create a file directly inside an existing "code" folder
#' use_quarto_template("10_draft_analysis", path = "code", subdir = FALSE)
#'
#' # Create a file inside a new subfolder within "code"
#' use_quarto_template("10_draft_analysis", path = "code", subdir = TRUE)
#' }
use_quarto_template <- function(file_name = "analysis",
                                path      = ".",
                                subdir    = FALSE,
                                open      = TRUE) {
  stopifnot(
    "`file_name` must be a single non-empty character string" =
      is.character(file_name) && length(file_name) == 1L && nchar(file_name) > 0
  )

  # ---- Resolve output path --------------------------------------------------
  if (subdir) {
    out_dir <- file.path(path, file_name)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
      message("Created sub-directory: '", out_dir, "'")
    }
  } else {
    out_dir <- path
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
      message("Created directory: '", out_dir, "'")
    }
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
