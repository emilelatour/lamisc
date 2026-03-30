#' Open the lamisc Quarto reference document
#'
#' Copies the lamisc Quarto syntax reference into a temporary location and
#' opens it for viewing or rendering. Use this as a living cheatsheet for
#' callout boxes, tabsets, columns, figure layouts, and other Quarto HTML
#' patterns. Copy snippets you need into your analysis, then close it.
#'
#' @param dest Character string. Directory to copy the reference file into.
#'   Defaults to a temporary directory so it never clutters your project.
#'   Set to `"."` to copy into the current working directory instead.
#' @param open Logical. If `TRUE` (default) the file is opened in the RStudio
#'   editor via [rstudioapi::navigateToFile()].
#'
#' @return The path to the reference `.qmd` file, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Open the reference doc from a temp directory (default)
#' use_quarto_reference()
#'
#' # Copy into current working directory instead
#' use_quarto_reference(dest = ".")
#' }
use_quarto_reference <- function(dest = tempdir(),
                                 open = TRUE) {

  # ---- Locate reference doc inside lamisc ----------------------------------

  ref_path <- system.file(
    "extdata/_extensions/lamisc/reference.qmd",
    package = "lamisc"
  )

  if (!nzchar(ref_path)) {
    stop(
      "Could not find the lamisc reference document. ",
      "Try re-installing lamisc with `devtools::install_github('emilelatour/lamisc')`."
    )
  }

  # ---- Copy to destination -------------------------------------------------

  out_file <- file.path(dest, "quarto_reference.qmd")

  file.copy(from = ref_path, to = out_file, overwrite = TRUE)
  message("Quarto reference copied to: '", out_file, "'")

  # ---- Open in editor ------------------------------------------------------

  if (open && rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(out_file)
  } else if (open) {
    utils::file.edit(out_file)
  }

  invisible(out_file)
}
