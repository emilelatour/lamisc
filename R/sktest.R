

#' @title
#' Skewness and kurtosis tests for normality
#'
#' @description
#'
#' This is an implementation of the `sktest` function that is available in
#' Stata. The function returns a test for normality based on skewness and on
#' kurtosis, and the a combination of the two tests into an overall test
#' statistic.
#'
#' All of this work here owes a big debt of gratitude and recognition to Stata
#' and to the autovar package. (link here: https://github.com/roqua/autovar)
#'
#' Skewness is a measure of the asymmetry of the probability distribution of a
#' random variable about its mean. It represents the amount and direction of
#' skew.  On the other hand, Kurtosis represents the height and sharpness of the
#' central peak relative to that of a standard bell curve.
#'
#' + Null hypothesis: the data follows a normal distribution.
#' + Alternative hypothesis: the data does not follow a normal distribution.
#'
#' @param data A data frame or tibble.
#' @param ... Unquoted variable names.
#' @param na_rm A logical value indicating whether NA values should be stripped
#'   before the computation proceeds. Default = `TRUE`
#' @param adjust A logical valules indicating whether overall chi-square
#'   statistic and p-value should be adjusted as proposed by Royston (1991).
#'   Default = `TRUE`.
#'
#' @importFrom dplyr bind_cols
#' @importFrom purrr map_df
#' @importFrom rlang exprs
#' @importFrom tibble tibble
#'
#' @references
#' + https://www.stata.com/manuals/rsktest.pdf
#' + D’Agostino, R. B., A. J. Belanger, and R. B. D’Agostino, Jr. 1990. A suggestion for using powerful and informative tests of normality. American Statistician 44: 316–321. https://doi.org/10.2307/2684359.
#' + Royston, P. 1991a. sg3.1: Tests for departure from normality. Stata Technical Bulletin 2: 16–17. Reprinted in Stata Technical Bulletin Reprints, vol. 1, pp. 101–104. College Station, TX: Stata Press.
#' + autovar package. https://github.com/roqua/autovar
#'
#' @return
#' A tibble with with variable names, number of observations, probability of
#' skewness, probability of kurtosis, chi-square test statistic and p-value for
#' the overall test of normality.
#'
#' @export
#'
#' @examples
#'
#' sktest(data = auto, mpg, trunk)
#' # We can reject the hypothesis that mpg is normally distributed, but we cannot
#' # reject the hypothesis that trunk is normally distributed, at least at the 12%
#' # level. The kurtosis for trunk is 2.19, and the p-value of 0.0445 shown in the
#' # table above indicates that it is significantly different from the kurtosis of
#' # a normal distribution at the 5% significance level. However, on the basis of
#' # skewness alone, we cannot reject the hypothesis that trunk is normally
#' # distributed.
#'
#' sktest(data = auto, mpg, trunk, adjust = FALSE)

sktest <- function(data, ..., na_rm = TRUE, adjust = TRUE) {

  dots <- rlang::exprs(...)
  dots <- as.character(dots)

  res <- purrr::map_df(.x = dots,
                       .f = ~ do_sktest(x = data[.x],
                                        na_rm = na_rm,
                                        adjust = adjust))

  tibble::tibble(var = dots) %>%
    dplyr::bind_cols(.,
                     res)


}




## Helper functions ----------------

do_sktest <- function(x, na_rm = TRUE, adjust = TRUE) {

  if (na_rm == TRUE) {
    x <- x[!is.na(x)]
  }


  g1 <- coefficient_of_skewness(x)
  b2 <- coefficient_of_kurtosis(x)
  n <- length(x)

  Z1 <- z_skewness(g1, n)
  Z2 <- z_kurtosis(b2, n)

  if (adjust == TRUE) {
    pp <- sktest_joint_p(Z1, Z2, n)
    adj_chi2 <- -2 * log(pp)
  } else {
    adj_chi2 <- Z1 ^ 2 + Z2 ^ 2
    pp <- pchisq(q = adj_chi2, df = 2, lower.tail = FALSE)
  }

  skr <- tibble::tibble(n = n,
                        pr_skewness = 2 - 2 * pnorm(abs(Z1)),
                        pr_kurtosis = 2 - 2 * pnorm(abs(Z2)),
                        adj_chi2 = adj_chi2,
                        p_value = pp)
  skr

}


z_skewness <- function(g1, n) {
  Y <- g1 * sqrt((n + 1) * (n + 3) / (6 * (n - 2)))
  B2g1 <- (3 * (n ^ 2 + 27 * n - 70) * (n + 1) * (n + 3)) / ((n - 2) * (n + 5) * (n + 7) * (n + 9))
  W2 <- -1 + sqrt(2 * (B2g1 - 1))
  a <- sqrt(2 / (W2 - 1))
  Z1 <- (1 / (sqrt(log(sqrt(W2))))) * log((Y / a) + sqrt((Y / a) ^ 2 + 1))
  Z1
}

z_kurtosis <- function(b2, n) {
  Eb2 <- (3 * (n - 1) / (n + 1))
  varb2 <- (24 * n * (n - 2) * (n - 3)) / ((n + 1) ^ 2 * (n + 3) * (n + 5))
  X <- (b2 - Eb2) / sqrt(varb2)
  sqrtB1b2 <-(6 * (n ^ 2 - 5 * n + 2) / ((n + 7) * (n + 9))) * sqrt((6 * (n + 3) * (n + 5)) / (n * (n - 2) * (n - 3)))
  A <- 6 + (8 / sqrtB1b2) * ((2 / sqrtB1b2) + sqrt(1 + (4 / (sqrtB1b2 ^ 2))))
  Z2 <- (1 / sqrt(2 / (9 * A))) * ((1 - (2 / (9 * A))) - ((1 - (2 / A)) / (1 + X * sqrt(2 / (A - 4)))) ^ (1 / 3))
  Z2
}

sktest_joint_p <- function(Z1, Z2, n) {
  K2 <- Z1 * Z1 + Z2 * Z2
  ZC2 <- -qnorm(exp(-0.5 * K2))
  logn <- log(n)
  cut <- 0.55 * (n ^ 0.2) - 0.21
  a1 <- (-5 + 3.46 * logn) * exp(-1.37 * logn)
  b1 <- 1 + (0.854 - 0.148 * logn) * exp(-0.55 * logn)
  b2mb1 <- 2.13 / (1 - 2.37 * logn)
  a2 <- a1 - b2mb1 * cut
  b2 <- b2mb1 + b1
  Z <- NULL
  if (ZC2 < -1) {
    Z <- ZC2
  } else if (ZC2 < cut) {
    Z <- a1 + b1 * ZC2
  } else {
    Z <- a2 + b2 * ZC2
  }
  P <- 1 - pnorm(Z)
  P
}

coefficient_of_skewness <- function(x) {
  m3 <- rth_moment_about_the_mean(x, 3)
  m2 <- rth_moment_about_the_mean(x, 2)
  ccoef <- m3 * m2 ^ (-3 / 2)
  ccoef
}

coefficient_of_kurtosis <- function(x) {
  m4 <- rth_moment_about_the_mean(x, 4)
  m2 <- rth_moment_about_the_mean(x, 2)
  ccoef <- m4 * m2 ^ (-2)
  ccoef
}

rth_moment_about_the_mean <- function(x, r) {
  mmean <- mean(x)
  n <- length(x)
  tsum <- 0
  for (i in 1:n) {
    if (i > n) {
      break
    }
    tsum <- tsum + (x[[i]] - mmean) ^ (r)
  }
  tsum <- (1 / n) * tsum
  tsum
}
