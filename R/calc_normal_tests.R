
#' @title
#' Tests of normality
#'
#' @description
#' Performs the following tests of normality given a data.frame and column:
#' + Shapiro-Francia test for the composite hypothesis of normality,
#' + (Robust) Jarque-Bera test of normality,
#' + Shapiro-Wilk test of normality,
#' + Anderson-Darling test for the composite hypothesis of normality,
#' + One--sample Kolmogorov-Smirnov test.
#'
#' Stata recommends to use the Shapiro-Francia test whenever possible. Note that
#' NAs are removed by default.
#'
#' + Null hypothesis: the data follows a normal distribution.
#' + Alternative hypothesis: the data does not follow a normal distribution.
#'
#' @param data A tibble or data frame.
#' @param var A numeric vector of data values.
#'
#' @importFrom DescTools ShapiroFranciaTest
#' @importFrom DescTools JarqueBeraTest
#' @importFrom nortest ad.test
#' @importFrom rlang enquo
#' @importFrom stats ks.test
#' @importFrom stats sd
#' @importFrom stats shapiro.test
#'
#' @return A tibble
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' data <- tibble::tibble(
#'   x = c(rnorm(99, mean = 5, sd = 3), NA),
#'   y = runif(100, min = 2, max = 4),
#'   z = rnorm(100, mean = 2, sd = 3))
#'
#' calc_normal_tests(data = data,
#'                   var = x)
#'
#' calc_normal_tests(data = data,
#'                   var = y)
#'
#'
#' calc_normal_tests(data = data,
#'                   var = z)

calc_normal_tests <- function(data, var) {

  var <- rlang::enquo(var)

  x <- data %>%
    dplyr::pull(!! var)

  x <- x[!is.na(x)]

  dplyr::bind_rows(
    wrap_shapiro_francia_test(x),
    wrap_jarque_bera_test(x),
    wrap_shapiro_wilkes_test(x),
    wrap_ad_test(x),
    wrap_ks_test(x)
  )



}



#### Helper functions --------------------------------

wrap_shapiro_francia_test <- function(x) {

  res <- DescTools::ShapiroFranciaTest(x = x)

  tibble::tibble(method = res$method,
                 stat_type = names(res$statistic),
                 statistic = res$statistic,
                 p_value = res$p.value)

}


wrap_jarque_bera_test <- function(x) {

  res <- DescTools::JarqueBeraTest(x = x)

  tibble::tibble(method = res$method,
                 stat_type = names(res$statistic),
                 statistic = res$statistic,
                 p_value = res$p.value)

}


wrap_shapiro_wilkes_test <- function(x) {

  res <- shapiro.test(x = x)

  tibble::tibble(method = res$method,
                 stat_type = names(res$statistic),
                 statistic = res$statistic,
                 p_value = res$p.value)

}


wrap_ks_test <- function(x) {

  res <- ks.test(unique(x), "pnorm", mean = mean(x), sd = sd(x))

  tibble::tibble(method = res$method,
                 stat_type = names(res$statistic),
                 statistic = res$statistic,
                 p_value = res$p.value)

}

wrap_ad_test <- function(x) {

  res <- nortest::ad.test(x)

  tibble::tibble(method = res$method,
                 stat_type = names(res$statistic),
                 statistic = res$statistic,
                 p_value = res$p.value)

}


