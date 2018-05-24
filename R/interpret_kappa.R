
#' @title
#' Interpretation of kappa statistic
#'
#' @description
#' This returns the interpretation of the strength of agreement determined by
#' the kappa statistic using the categorization by Landis and Koch (1977)
#'
#' @param kappa Numeric; kappa statistic
#'
#' @import dplyr
#'
#' @return A string
#' @export
#'
#' @examples
#' interpret_kappa(0.5)
#' interpret_kappa(0.896)
#' interpret_kappa(-0.896)
#' interpret_kappa(0.60)
#'
interpret_kappa <- function(kappa) {

  dplyr::case_when(
    kappa < 0.00 ~ "poor",
    kappa < 0.20 ~ "slight",
    kappa < 0.40 ~ "fair",
    kappa < 0.60 ~ "moderate",
    kappa < 0.80 ~ "substantial",
    kappa < 1.00 ~ "almost perfect",
    kappa == 1.00 ~ "perfect"
  )

}



