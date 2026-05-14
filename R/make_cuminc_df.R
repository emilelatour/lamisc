#' Calculate cumulative incidence from Kaplan-Meier
#'
#' Returns a tibble of the KM survival curve and the corresponding cumulative
#' incidence (1 - S(t)) with confidence intervals on both scales, at every
#' event/censoring time. Optionally stratified by one or more grouping variables.
#'
#' Cumulative-incidence CIs are derived by flipping the KM survival bounds
#' (`cum_inc_lower = 1 - upper_ci`, `cum_inc_upper = 1 - lower_ci`). For a true
#' competing-risks analysis, use [tidycmprsk::cuminc()] instead.
#'
#' @param data A data frame.
#' @param time Unquoted name of the time-to-event variable.
#' @param event Unquoted name of the event indicator (0 = censored, 1 = event).
#' @param ... Optional unquoted grouping variables.
#' @param type Type of censoring; passed to [survival::Surv()].
#' @param extend Passed to [survival::summary.survfit()].
#'
#' @return A tibble with columns: grouping variables (if any), `time`, `n_risk`,
#'   `n_event`, `surv`, `lower_ci`, `upper_ci`, `cum_inc`, `cum_inc_lower`,
#'   `cum_inc_upper`.
#'
#' @export
#'
#' @examples
#' library(survival)
#'
#' # Single curve
#' make_cuminc_df(aml, time, status)
#'
#' # Stratified
#' make_cuminc_df(aml, time, status, x)
make_cuminc_df <- function(data, time, event, ...,
                           type = "right", extend = FALSE) {

  # Silence R CMD check
  .surv_time <- .surv_event <- NULL

  df <- data %>%
    dplyr::mutate(
      .surv_time  = {{ time }},
      .surv_event = {{ event }}
    )

  fit_one_group <- function(d) {
    fit <- survival::survfit(
      survival::Surv(time = .surv_time, event = .surv_event, type = type) ~ 1,
      data = d
    )
    s <- summary(fit, times = sort(unique(c(0, fit$time))), extend = extend)

    tibble::tibble(
      time          = s$time,
      n_risk        = s$n.risk,
      n_event       = s$n.event,
      surv          = s$surv,
      lower_ci      = s$lower,
      upper_ci      = s$upper,
      cum_inc       = 1 - s$surv,
      cum_inc_lower = 1 - s$upper,
      cum_inc_upper = 1 - s$lower
    )
  }

  df %>%
    dplyr::group_by(...) %>%
    tidyr::nest() %>%
    dplyr::mutate(res = purrr::map(data, fit_one_group)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(res) %>%
    dplyr::ungroup()
}
