
#### Packages --------------------------------

library(dplyr)


#' @title
#' Calculate confidence interval for crude incidence rate
#'
#' @description
#' 7 different methods that I found online for calculating the confidence
#' interval for a crude incidence rate. Note that these are all two sided. Most
#' of these come from https://www.openepi.com and their documentation and formulas.
#'
#' Mid-P exact test seems to be the preferred method
#'
#' + "Mid-P exact test" using Miettinen's (1974d) modification, as described in Epidemiologic Analysis with a Programmable Calculator, 1979.
#' + "Fisher's exact test" based on the formula (Armitage,1971; Snedecor & Cochran,1965) as described in Epidemiologic Analysis with a Programmable Calculator, 1979.
#' + "Exact Poisson is the same as the Fisher's exact but calculated differently.
#' + "Normal approximation" to the Poisson distribution as described by Rosner, Fundamentals of Biostatistics (5th Ed).
#' + "Byar approx. Poisson" as described in Rothman and Boice, Epidemiologic Analysis with a Programmable Calculator, 1979.
#' + "Rothman/Greenland" as described in Rothman and Greenland, Modern Epidemiology (2nd Ed).
#' + "CCRB" comes from the follwing website and they do not offer documentation for their methods: http://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20single%20rate.htm
#'
#' @param a Number of clinical events
#' @param N Person-time at risk
#' @param pt_units Factor to multiply rate by to get Person-time; default is 100.
#' @param alpha Significance level for two-sided confidence interval
#' @param interval a vector containing the end-points of the interval to be
#'   searched for the root. The function `base::uniroot()` is used to solve for
#'   some confidence intervals iteratively.
#'
#' @references
#' http://epid.blogspot.com/2012/08/how-to-calculate-confidence-interval-of.html
#' https://www.openepi.com/PDFDocs/PersonTime1Doc.pdf
#' https://seer.cancer.gov/seerstat/WebHelp/Rate_Algorithms.htm
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate_at
#' @importFrom dplyr vars
#' @importFrom stats qchisq
#' @importFrom stats uniroot
#' @importFrom tibble tibble
#'
#'
#' @return
#' A tbl_df
#'
#' @export
#'
#' @examples
#' # options(pillar.sigfig = 3)
#' ir_calc(a = 18,
#'         N = 352 + 10.5,
#'         alpha = 0.05)
#'
#' #### From  https://www.openepi.com --------------------------------
#'
#' # Person-Time Rate and 95% Confidence Intervals
#' # Per 100 Person-Time Units
#' # Number of cases:	18
#' # Person-Time:	362.5
#' #
#' #                      Lower CL	  Rate	Upper CL
#' # Mid-P exact test	       3.035	 4.966	   7.696
#' # Fisher's exact test	   2.943		         7.848
#' # Normal approximation	   2.672		         7.259
#' # Byar approx. Poisson	   2.941		         7.848
#' # Rothman/Greenland	     3.129		         7.881

ir_calc <- function(a,
                    N,
                    pt_units = 100,
                    alpha = 0.05,
                    interval = c(0, 10000000)) {

  # options(pillar.sigfig = 6)

  dplyr::bind_rows(
    ir_calc_mid_p(a, N, pt_units, alpha, interval),
    ir_calc_fisher(a, N, pt_units, alpha, interval),
    ir_calc_exact_poisson(a, N, pt_units, alpha),
    ir_calc_normal(a, N, pt_units, alpha),
    ir_calc_byar(a, N, pt_units, alpha),
    ir_calc_roth_green(a, N, pt_units, alpha),
    ir_calc_ccrb(a, N, pt_units, alpha),
  )


}





#### Helper functions --------------------------------

## Mid-P exact test ----------------

ir_calc_mid_p <- function(a,
                          N,
                          pt_units = 100,
                          alpha = 0.05,
                          interval = c(0, 10000000)) {

  # https://www.openepi.com/Menu/OE_Menu.htm
  # https://cran.r-project.org/web/packages/exact2x2/vignettes/midpAdjustment.pdf

  k <- 0:(a - 1)

  lower_bound <- function(x) {
    (1 / 2) * exp(-x) * (x ^ a) / factorial(a) +
      sum(exp(-x) * (x ^ k) / factorial(k)) - (1 - alpha / 2)
  }

  upper_bound <- function(x) {
    (1 / 2) * exp(-x) * (x ^ a) / factorial(a) +
      sum(exp(-x) * (x ^ k) / factorial(k)) - (alpha / 2)
  }


  tibble::tibble(
    method = "Mid-P exact test",
    number_of_cases = a,
    person_time = N,
    rate = a / N,
    se = NA_real_,
    lower_ci = uniroot(lower_bound,
                       interval = interval)$root / N,
    upper_ci = uniroot(upper_bound,
                       interval = interval)$root / N
  ) %>%
    mutate_at(.vars = dplyr::vars(rate,
                                  lower_ci,
                                  upper_ci),
              .funs = list(~ pt_units * .))


}

# ir_calc_mid_p(a = 18,
#               N = 352 + 10.5,
#               alpha = 0.05)


## Fisher's exact test ----------------

# Turns out to be the same as the Exact Poisson...

ir_calc_fisher <- function(a,
                           N,
                           pt_units = 100,
                           alpha = 0.05,
                           interval = c(0, 10000000)) {

  k_l <- 0:(a - 1)
  k_u <- 0:a

  lower_bound <- function(x) {
    sum(exp(-x) * (x ^ k_l) / factorial(k_l)) - (1 - alpha / 2)
  }

  upper_bound <- function(x) {
    sum(exp(-x) * (x ^ k_u) / factorial(k_u)) - (alpha / 2)
  }


  tibble::tibble(
    method = "Fisher's exact test",
    number_of_cases = a,
    person_time = N,
    rate = a / N,
    se = NA_real_,
    lower_ci = uniroot(lower_bound,
                       interval = interval)$root / N,
    upper_ci = uniroot(upper_bound,
                       interval = interval)$root / N
  ) %>%
    mutate_at(.vars = dplyr::vars(rate,
                                  lower_ci,
                                  upper_ci),
              .funs = list(~ pt_units * .))

}

# ir_calc_fisher(a = 18,
#                N = 352 + 10.5,
#                alpha = 0.05)


## Exact Poisson ----------------

ir_calc_exact_poisson <- function(a,
                                  N,
                                  pt_units = 100,
                                  alpha = 0.05) {

  # http://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20single%20rate.htm
  # https://www.statsdirect.com/help/rates/poisson_rate_ci.htm
  # https://www.openepi.com/PersonTime1/PersonTime1.htm
  # https://www.openepi.com/PDFDocs/ProportionDoc.pdf

  # http://epid.blogspot.com/2012/08/how-to-calculate-confidence-interval-of.html
  # Ulm, 1990

  tibble::tibble(
    method = "Exact Poisson",
    number_of_cases = a,
    person_time = N,
    rate = a / N,
    lower_ci = qchisq(p = alpha / 2,
                      df = 2 * a) / 2,
    upper_ci = qchisq(p = 1 - alpha / 2,
                      df = 2 * (a + 1)) / 2
  ) %>%
    mutate(lower_ci = lower_ci / N,
           upper_ci = upper_ci / N) %>%
    mutate_at(.vars = dplyr::vars(rate,
                                  lower_ci,
                                  upper_ci),
              .funs = list(~ pt_units * .))

}

# ir_calc_exact_poisson(a = 18,
#                       N = 352 + 10.5,
#                       alpha = 0.05)


## Normal Approximation ----------------

ir_calc_normal <- function(a,
                           N,
                           pt_units = 100,
                           alpha = 0.05) {

  # If a is large enough
  # http://epid.blogspot.com/2012/08/how-to-calculate-confidence-interval-of.html

  z_score <- qnorm(p = 1 - alpha / 2,
                   mean = 0,
                   sd = 1,
                   lower.tail = TRUE)

  rate <- a / N
  se <- sqrt(a / N ^ 2)

  tibble::tibble(
    method = "Normal approximation",
    number_of_cases = a,
    person_time = N,
    rate = rate,
    se = se,
    lower_ci = rate - z_score * se,
    upper_ci = rate + z_score * se
  ) %>%
    mutate_at(.vars = dplyr::vars(rate,
                                  lower_ci,
                                  upper_ci),
              .funs = list(~ pt_units * .))


}

# ir_calc_normal(a = 18,
#                N = 352 + 10.5,
#                alpha = 0.05)


## Byar approx. Poisson ----------------

ir_calc_byar <- function(a,
                         N,
                         pt_units = 100,
                         alpha = 0.05) {


  z_score <- qnorm(p = 1 - alpha / 2,
                   mean = 0,
                   sd = 1,
                   lower.tail = TRUE)

  lower_ci <- a * (1 - 1 / (9 * a) - (z_score / 3) * sqrt(1 / a)) ^ 3
  upper_ci <- (a + 1) * (1 - 1 / (9 * (a + 1)) + (z_score / 3) * sqrt(1 / (a + 1))) ^ 3


  tibble::tibble(
    method = "Byar approx. Poisson",
    number_of_cases = a,
    person_time = N,
    rate = a / N,
    se = NA_real_,
    lower_ci = lower_ci / N,
    upper_ci = upper_ci / N
  ) %>%
    mutate_at(.vars = dplyr::vars(rate,
                                  lower_ci,
                                  upper_ci),
              .funs = list(~ pt_units * .))


}

# ir_calc_byar(a = 18,
#               N = 352 + 10.5,
#               alpha = 0.05)


## Rothman-Greenland ----------------

ir_calc_roth_green <- function(a,
                               N,
                               pt_units = 100,
                               alpha = 0.05) {


  z_score <- qnorm(p = 1 - alpha / 2,
                   mean = 0,
                   sd = 1,
                   lower.tail = TRUE)

  rate <- a / N
  # se <- sqrt((1 - rate) / a)
  se <- sqrt(1 / a)

  tibble::tibble(
    method = "Rothman-Greenland",
    number_of_cases = a,
    person_time = N,
    rate = rate,
    se = se,
    lower_ci = exp(log(rate) - (z_score * se)),
    upper_ci = exp(log(rate) + (z_score * se))
  ) %>%
    mutate_at(.vars = dplyr::vars(rate,
                                  lower_ci,
                                  upper_ci),
              .funs = list(~ pt_units * .))


}

# ir_calc_roth_green(a = 18,
#                    N = 352 + 10.5,
#                    alpha = 0.05)




## http://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20single%20rate.htm ----------------


# http://epid.blogspot.com/2012/08/how-to-calculate-confidence-interval-of.html
# https://stats.stackexchange.com/questions/301777/how-to-calculate-confidence-interval-of-incidence-rate-under-the-poisson-distrib



ir_calc_ccrb <- function(a,
                         N,
                         pt_units = 100,
                         alpha = 0.05) {

  # http://www2.ccrb.cuhk.edu.hk/stat/confidence%20interval/CI%20for%20single%20rate.htm
  # https://www.statsdirect.com/help/rates/poisson_rate_ci.htm

  # They define their standard error as sqrt((1 - r) / a) where r = a / N with
  # no further documentation

  z_score <- qnorm(p = 1 - alpha / 2,
                   mean = 0,
                   sd = 1,
                   lower.tail = TRUE)

  tibble::tibble(
    method = "CCRB",
    number_of_cases = a,
    person_time = N,
    rate = a / N,
    se = sqrt((1 - rate) / a),
    lower_ci = log(rate) - (z_score * se),
    upper_ci = log(rate) + (z_score * se)) %>%
    mutate_at(.vars = dplyr::vars(lower_ci, upper_ci),
              .funs = list(~ exp(.))) %>%
    mutate_at(.vars = dplyr::vars(rate,
                                  lower_ci,
                                  upper_ci),
              .funs = list(~ pt_units * .))


}

