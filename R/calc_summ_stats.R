
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
#'
#' @rdname calc_summ_stats
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
#'   calc_summ_stats_t(
#'     heightm = height / 100,
#'     bmi = mass / heightm^2
#'   )
#'
#' # Grouped by gender
#' starwars %>%
#'   group_by(gender) %>%
#'   calc_summ_stats_t(
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
    # dplyr::transmute(...) %>%
    tidyr::gather(key = "variable",
                  value = "value",
                  ...,
                  -dplyr::one_of(dplyr::group_vars(.))) %>%
    group_by(.data$variable, add = TRUE) %>%
    summarise_at(vars(.data$value),
                 summary_functions) %>%
    mutate(range = .data$p100 - .data$p0,
           CV = 100 * .data$sd / .data$mean)
}

#' @rdname calc_summ_stats
#' @export
calc_summ_stats_t <- function(.data, ...) {
  .data %>%
    dplyr::transmute(...) %>%
    tidyr::gather(key = "variable",
                  value = "value",
                  -dplyr::one_of(dplyr::group_vars(.))) %>%
    group_by(.data$variable, add = TRUE) %>%
    summarise_at(vars(.data$value),
                 summary_functions) %>%
    mutate(range = .data$p100 - .data$p0,
           CV = 100 * .data$sd / .data$mean)
}


#### Function to calc summary stas --------------------------------

summary_functions <- list(
  n = ~ length(.),
  complete = ~ sum(!is.na(.)),
  missing =  ~ sum(is.na(.)),
  mean =     ~ mean(., na.rm = TRUE),
  sd =       ~ sd(., na.rm = TRUE),
  p0 =       ~ min(., na.rm = TRUE),
  p25 =      ~ quantile(., probs = 0.25, na.rm = TRUE),
  p50 =      ~ quantile(., probs = 0.50, na.rm = TRUE),
  p75 =      ~ quantile(., probs = 0.75, na.rm = TRUE),
  p100 =     ~ max(., na.rm = TRUE)
)
