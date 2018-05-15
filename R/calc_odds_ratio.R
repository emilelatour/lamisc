
#' @title
#' A simple function to calculate an odds ratio and the asymptotic confidence
#' interval.
#'
#' @description
#' Given a data frame and two columns, the function will calculate and odds
#' ratio and the asymptotic (Wald) confidence interval. Note the variables
#' should have two levels so that a 2x2 table could be formed.
#'
#' Interpretation: The odds of `x` among those with `y` is OR time the
#' odds of `x` among those without `y`.
#'
#' `DescTools::OddsRatio()` might be better for general use. Or the great
#' package `vcd` also has a function.
#'
#' @param df A data frame or tibble
#' @param x The outcome variable of interest; the disease; the horizontal
#'   variable at the top of a 2x2 table.
#' @param y The factor variable of interest; the exposure; the vertical
#'   variable along the side of a 2x2 table.
#' @param pad_zeros If `TRUE` add 0.5 to any zero cells in order to make the
#'   calculations work. Kind of an old epidemiologists trick. More sophisticated
#'   statistical methods exists to address this.
#' @param conf_level Confidence levels
#'
#' @import rlang
#' @importFrom stats qnorm
#'
#'
#' @return A tibble
#' @export
#'
#' @examples
#' phs <- matrix(c(189, 10845, 104, 10933), byrow = TRUE, ncol = 2)
#' dimnames(phs) <- list(Group = c("Placebo", "Aspirin"), MI = c("Yes", "No"))
#' phs # a matrix
#' # convert it to a data frame
#' phs_df <- lamisc::counts_to_cases(phs)
#' calc_odds_ratio(df = phs_df, x = Group, y = MI)
#'
#' simple_df <- tibble::tibble(
#'   disease = sample(c("Present", "Absent"),
#'                    size = 100,
#'                    replace = TRUE,
#'                    prob = c(0.30, 0.70)),
#'   exposure = sample(c("Exposed", "Not exposed"),
#'                     size = 100,
#'                     replace = TRUE,
#'                     prob = c(0.60, 0.40))
#' )
#' janitor::tabyl(dat = simple_df, exposure, disease)
#' calc_odds_ratio(df = simple_df, x = disease, y = exposure)
#'



calc_odds_ratio <- function(df,
                            x,
                            y,
                            pad_zeros = FALSE,
                            conf_level = 0.95) {

  tab <- lamisc::make_table(df = df,
                            x_var = !! rlang::enquo(x),
                            y_var = !! rlang::enquo(y),
                            x_lvls = NULL,
                            y_lvls = NULL,
                            labs = c(NA, NA),
                            useNA = "ifany")

  if (pad_zeros) {
    if (any(tab == 0)) tab <- tab + 0.5
  }

  theta <- tab[1, 1] * tab[2, 2] / (tab[2, 1] * tab[1, 2])
  ASE <- sqrt(sum(1 / tab))
  CI <- exp(log(theta) + c(-1, 1) * qnorm(0.5 * (1 + conf_level)) * ASE)

  tibble::tibble(estimate = theta,
                 ASE = ASE,
                 conf_low = CI[[1]],
                 conf_high = CI[[2]],
                 conf_level = conf_level)
}
