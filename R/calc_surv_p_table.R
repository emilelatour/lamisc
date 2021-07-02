
#' @title
#' Calculate time-to-event estimates
#'
#' @description
#' Takes a `survfit` object and returns a tibble with proportion surviving at
#' given times and confidence intervals.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
#'
#' @param fit `survfit` object
#' @param times A numeric vector
#' @param all_group_name String name for all records. Default is "All subjects".
#'
#' @return A tibble
#' @export
#'
#' @examples
#' library(survival)
#' library(dplyr)
#' head(aml)
#'
#' fit <- survival::survfit(
#'   survival::Surv(time = time,
#'                  event = status) ~ x,
#'   data = aml)
#'
#' calc_surv_p_table(fit)
#' calc_surv_p_table(fit, times = c(6, 18, 30))
#'
#' fit <- survival::survfit(
#'   survival::Surv(time = time,
#'                  event = status) ~ 1,
#'   data = aml)
#'
#' calc_surv_p_table(fit)
#' calc_surv_p_table(fit, times = c(6, 18, 30))
#'
calc_surv_p_table <- function(fit,
                              times = c(0, 12, 24, 36, 48),
                              all_group_name = "All subjects") {

  lower <- n_event <- n_risk <- time <- upper <- NULL


  if ("strata" %in% names(fit)) {

    fit %>%
      summary(times = times,
              extend = TRUE) %>%
      .[c("time", "strata", "n.risk", "n.event", "surv", "lower", "upper")] %>%
      tibble::as_tibble() %>%
      # mutate(strata = gsub(".*=", "", strata),
      #        strata = factor(strata)) %>%
      dplyr::mutate(strata = gsub(".*=", "", strata)) %>%
      janitor::clean_names() %>%
      dplyr::rename(lower_ci = lower, upper_ci = upper)

  } else {

    fit %>%
      summary(times = times,
              extend = TRUE) %>%
      .[c("time", "n.risk", "n.event", "surv", "lower", "upper")] %>%
      tibble::as_tibble() %>%
      dplyr::mutate(strata = all_group_name) %>%
      janitor::clean_names() %>%
      dplyr::select(time, strata, n_risk, n_event, surv, lower_ci = lower, upper_ci = upper)
  }

}
