
#' @title
#' Provides the Variance-covariance parameters estimates
#'
#' @description
#' User-written function to output the covariance parameters
#'
#' @references
#' \url{https://stats.idre.ucla.edu/r/examples/alda/r-applied-longitudinal-data-analysis-ch-7/}
#'
#' @param glsob An object of class "gls" representing the linear model fit
#' @param cov Default is TRUE. Not sure how this is used
#' @param ... Additional arguments
#'
#' @importFrom nlme gls
#' @importFrom nlme corMatrix
#' @importFrom nlme corSymm
#' @importFrom nlme varIdent
#' @importFrom stats coef
#'
#' @return A list with correlation matrix, variance structure, and covariance
#'   matrix.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(readr)
#' library(nlme)
#'
#' opposites <- readr::read_csv(
#'   file = "https://stats.idre.ucla.edu/stat/r/examples/alda/data/opposites_pp.txt",
#'   col_names = TRUE)
#' dplyr::glimpse(opposites)
#'
#' unstruct <- gls(opp ~ time * ccog,
#'                 opposites,
#'                 correlation = corSymm(form = ~ 1 |
#'                                         id),
#'                 weights = varIdent(form = ~ 1 | wave),
#'                 method = "REML")
#'
#' corandcov(unstruct)

corandcov <- function(glsob, cov = TRUE, ...) {

  corm <- nlme::corMatrix(glsob$modelStruct$corStruct)[[5]]

  varstruct <- glsob$modelStruct$varStruct

  varests <- stats::coef(varstruct, uncons = FALSE, allCoef = TRUE)
  covm <- corm * glsob$sigma ^ 2 * t(t(varests)) %*% t(varests)

  list(correlation_matrix = corm,
       variance_structure = varstruct,
       covariance_matrix = covm)

}
