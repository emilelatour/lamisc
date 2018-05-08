
#' @title
#' Install missing packages from bioconductor, CRAN, and GitHub
#'
#' @description
#' Makes installing missing packages a little less painful.
#'
#' Function by Ernest Omane-Kodie.
#'
#' @references \url{https://www.eokodie.com/blog/installing-missing-packages-from-bioconductor-cran-and-github/}
#'
#' @param package The name of a package to intall
#'
#' @import dplyr
#' @import rlang
#' @importFrom tidyr separate
#' @importFrom devtools install_github
#' @importFrom glue glue
#' @importFrom jsonlite fromJSON
#' @importFrom tools CRAN_package_db
#' @importFrom utils install.packages
#'
#' @return Invisible `NULL`.
#' @export
#'
#' @examples
#' install_missing_pkg("remedy")

install_missing_pkg <- function(package) {
  url <- glue::glue("http://rpkg-api.gepuro.net/rpkg?q={package}")
  cran_pkgs <- tools::CRAN_package_db()$Package
  gh_pkgs <- jsonlite::fromJSON(url)
  source("http://bioconductor.org/biocLite.R")
  bioc_pkgs = all_group()

  if (is.null(nrow(gh_pkgs)) & !(package %in% union(cran_pkgs, bioc_pkgs))) {
    stop(glue::glue("{package} is not available on CRAN, Github or Bioconductor"))
  }
  # if (is.null(nrow(gh_pkgs)) | !(package %in% union(cran_pkgs, bioc_pkgs))) {
  #   stop("`package` is not available on CRAN, Github or Bioconductor")
  # }
  # install from CRAN
  if (package %in% cran_pkgs) {
    install.packages(package)
    # install from Bioconductor
  } else if (package %in% bioc_pkgs) {
    biocLite(package, suppressUpdates = TRUE)
  }
  # install from Github
  else {
    gh_pkg <- gh_pkgs %>%
      tidyr::separate(col = .data$pkg_name,
                      into = c("repo", "pkg"),
                      sep = "/",
                      remove = FALSE) %>%
      dplyr::mutate(available = package %in% cran_pkgs) %>%
      dplyr::filter(.data$pkg == package)
    devtools::install_github(gh_pkg$pkg_name[1])
  }
}
