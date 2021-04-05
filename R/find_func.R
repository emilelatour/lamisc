#' @title
#' Identifying the package of a function
#'
#' @description
#' I found this handy function to identify which package an R package belongs
#' to. I found this function on Sebastian Sauer's \href{https://sebastiansauer.github.io/finds_funs/}{blog}.
#'
#'
#' @param f name of function for which the package(s) are to be identified; a
#'   character string.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom utils help.search
#' @importFrom utils installed.packages
#'
#' @return
#' A tibble.
#'
#' @export
#'
#'@examples
#'\dontrun{
#' find_func("width")
#'}


find_func <- function(f) {
  # Returns dataframe with two columns:
    # `package_name`: packages(s) which the function is part of (chr)
    # `builtin_package`:  whether the package comes with standard R (a 'builtin'  package)

  # Arguments:
    # f: name of function for which the package(s) are to be identified.


  if ("tidyverse" %in% rownames(utils::installed.packages()) == FALSE) {
    cat("tidyverse is needed for this fuction. Please install. Stopping")
    stop()}

  # suppressMessages(library(tidyverse))

  # search for help in list of installed packages
  help_installed <- utils::help.search(paste0("^", f, "$"), agrep = FALSE)

  # extract package name from help file
  pckg_hits <- help_installed$matches[, "Package"]

  if (length(pckg_hits) == 0) {
    pckg_hits <- "No_results_found"
  }

  # get list of built-in packages

  pckgs <- utils::installed.packages() %>%
    tibble::as_tibble()

  builtin_pckgs_df <- pckgs %>%
    dplyr::filter(.data$Priority %in% c("base","recommended")) %>%
    dplyr::select(.data$Package) %>%
    dplyr::distinct()

  # check for each element of 'pckg hit' whether its built-in and loaded (via match). Then print results.

  results <- tibble::tibble(
    package_name = pckg_hits,
    builtin_pckage = match(pckg_hits, builtin_pckgs_df$Package, nomatch = 0) > 0,
    loaded = match(paste("package:", pckg_hits, sep = ""), search(), nomatch = 0) > 0
  )

  return(results)

}
