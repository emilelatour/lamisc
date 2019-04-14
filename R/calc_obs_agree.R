

#' @title Calculate observed agreement and confidence interval
#'
#' @description
#' This is really just a wrapper for the `irr::agree()` function from that
#' package, but I wanted to calculate the confidence interval also.
#'
#' @details
#' Missing data are omitted in a listwise way.
#' Using extended percentage agreement (tolerance!=0) is only possible for
#' numerical values. If tolerance equals 1, for example, raters differing by one
#' scale degree are interpreted as agreeing.
#'
#' @param data A data frame or tibble, n subjects (rows) and m raters (columns)
#' @param ... Variables or columns of ratings
#' @param alpha Numeric; two-sided type one error, default = 0.05
#' @param tolerance Used in the `irr::agree()` function; number of successive
#'   rating categories that should be regarded as rater agreement (see details).
#'
#' @import dplyr
#' @import rlang
#' @importFrom tibble tibble
#' @importFrom irr agree
#' @importFrom stats qnorm
#' @importFrom rlang .data
#'
#'
#' @return A tibble with the following columns
#' \item{N}{Number of subjects}
#' \item{po}{Percent agreement among raters}
#' \item{se_po}{Standard error of the proportion}
#' \item{conf_low}{Lower asymptotic confidence interval}
#' \item{conf_high}{Upper asymptotic confidence interval}
#'
#' @export
#'
#' @examples
#' library(irr)
#' data("video")
#' calc_obs_agree(data = video, rater1:rater4)
#' calc_obs_agree(data = video, rater1:rater2)
calc_obs_agree <- function(data, ..., alpha = 0.05, tolerance = 0) {

  vars <- rlang::enquos(...)

  pct_agree <- data %>%
    dplyr::select(!!! vars) %>%
    irr::agree(., tolerance = tolerance)
  # SE(po) = sqrt[po(1 - po)/N]

  tibble::tibble(
    N = pct_agree$subjects,
    po = pct_agree$value / 100,
    se_po = sqrt(.data$po * (1 - .data$po) / .data$N),
    lower_ci = .data$po + c(-1) * qnorm(1 - alpha / 2) * .data$se_po,
    upper_ci = .data$po + c(1) * qnorm(1 - alpha / 2) * .data$se_po) %>%
    dplyr::mutate_at(.vars = vars(.data$po:.data$upper_ci),
                     .funs = funs(lamisc::roundr(., d = 3)))
}
