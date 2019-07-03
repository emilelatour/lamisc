#' @title
#' Set your R library paths where you want when you want
#'
#' @description
#' Credit to this one goes to Miles McBain (see link in references). An
#' alternate to `.libPaths()` that will let you set your R library paths to
#' precisely whatever you choose, whenever you like.
#'
#' @param lib_vec Character string with path to library
#'
#' @export
#'
#' @references
#' \href{https://milesmcbain.xyz/hacking-r-library-paths}{Hacking R's library paths}
#'
#' @examples \dontrun{
#' .libPaths()
#' set_lib_paths("~/code/library")
#' .libPaths()
#' }
set_lib_paths <- function(lib_vec) {

  lib_vec <- normalizePath(lib_vec, mustWork = TRUE)

  shim_fun <- .libPaths
  shim_env <- new.env(parent = environment(shim_fun))
  shim_env$.Library <- character()
  shim_env$.Library.site <- character()

  environment(shim_fun) <- shim_env
  shim_fun(lib_vec)
}
