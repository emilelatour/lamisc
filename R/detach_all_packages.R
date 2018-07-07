#' @title
#' Detach all currently loaded packages
#'
#' @description
#' I found this in some example code online and thought that it was super handy
#' seeming and wanted to save it for later.
#'
#' @references
#' /url{https://github.com/tomasu909/Tidy-Tuesday-Submissions/blob/master/tidytuesday2.Rmd}
#'
#' @examples \dontrun{
#' # Load some packages
#' library(dplyr)
#' library(tibble)
#' library(ggplot2)
#' library(readr)
#' # List of loaded packages -- before
#' (.packages())
#'
#' # Detach all packages
#' detach_all_packages()
#'
#' # List of loaded packages -- after
#' (.packages())
#' }
#'
detach_all_packages <- function() {
  basic_packages_blank <- c("stats",
                            "graphics",
                            "grDevices",
                            "utils",
                            "datasets",
                            "methods",
                            "base")

  basic_packages <- paste("package:", basic_packages_blank, sep = "")

  package_list <-
    search()[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]

  package_list <- base::setdiff(package_list, basic_packages)

  if (length(package_list) > 0) {
    for (package in package_list) {
      detach(package, character.only = TRUE)
    }
  }

}


