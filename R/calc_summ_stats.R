
#' Title
#' Calculate summary / descriptive statistics
#'
#' @description
#' This function provide some brief overview statstics for selected variables of
#' a tbl_df. Number of observations (n), complete observations (complete),
#' missing observations (missing); mean, standard deviation (sd), minimum value
#' (p0), maximum value (p100), median (p50), interquartile rane (p25, p75).
#'
#' @import dplyr
#' @importFrom tidyr gather
#' @importFrom rlang .data
#'
#' @param .data A tbl
#' @param ... Variables to summarise
#'
#' @return A tbl
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)  # to get the starwars data set
#'
#' # descriptive stats for height and mass
#' starwars %>%
#'   calc_summ_stats(
#'     height, mass
#'   )
#'
#' # Grouped by gender
#' starwars %>%
#'   group_by(gender) %>%
#'   calc_summ_stats(
#'     height, mass
#'   )
#'
#' # Derive variables within function then summarise
#' starwars %>%
#'   calc_summ_stats(
#'     heightm = height / 100,
#'     bmi = mass / heightm^2
#'   )
#'
#' # Grouped by gender
#' starwars %>%
#'   group_by(gender) %>%
#'   calc_summ_stats(
#'     heightm = height / 100,
#'     bmi = mass / heightm^2
#'   )
#'
#' # Doesn't work with factors/characters as of 2018-01-19
#' # starwars %>%
#' #   calc_summ_stats(
#' #   height, mass, gender
#' #  )
#'

calc_summ_stats <- function(.data, ...) {
  .data %>%
    dplyr::transmute(...) %>%
    tidyr::gather(key = "variable",
                  value = "value",
                  -dplyr::one_of(dplyr::group_vars(.))) %>%
    group_by(.data$variable, add = TRUE) %>%
    summarise_at(vars(.data$value), summary_functions) %>%
    dplyr::rename(n = .data$`dplyr::n`) %>%
    mutate(range = .data$p100 - .data$p0)
}



#### Function to calc summary stas --------------------------------

summary_functions <- list(
  n ~ dplyr::n(),
  complete = function(x) sum(!is.na(x)),
  missing = function(x) sum(is.na(x)),
  mean = function(x) mean(x, na.rm = TRUE),
  sd = function(x) sd(x, na.rm = TRUE),
  p0 = function(x) min(x, na.rm = TRUE),
  p25 = function(x) quantile(x, probs = 0.25, na.rm = TRUE),
  p50 = function(x) quantile(x, probs = 0.50, na.rm = TRUE),
  p75 = function(x) quantile(x, probs = 0.75, na.rm = TRUE),
  p100 = function(x) max(x, na.rm = TRUE)
)
