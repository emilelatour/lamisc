
#' @title
#' Helper function for `calc_cuminc_df`
#'
#' @description
#' Just a helper function to make things a little cleaner and easier in the
#' function `calc_cuminc_df`.
#'
#' @param data A data frame or tibble
#' @param time Vector of integer event times. For right censored data, this is
#'   the follow up time. For interval data, the first argument is the starting
#'   time for the interval.
#' @param event The status indicator, normally 0=alive, 1=dead. Other choices
#'   are TRUE/FALSE (TRUE = death) or 1/2 (2=death). For interval censored data,
#'   the status indicator is 0=right censored, 1=event at time, 2=left censored,
#'   3=interval censored. For multiple enpoint data the event variable will be a
#'   factor, whose first level is treated as censoring. Although unusual, the
#'   event indicator can be omitted, in which case all subjects are assumed to
#'   have an event.
#' @param type character string specifying the type of censoring. Possible
#'   values are "right", "left", "counting", "interval", "interval2" or
#'   "mstate".
#' @param extend logical value: if TRUE, prints information for all specified
#'   times, even if there are no subjects left at the end of the specified
#'   times. This is only valid if the times argument is present.
#'
#'
#'
#' @return A tibble
.calc_surv <- function(data, time, event,
                       type = "right", extend = FALSE) {

  time_enq <- rlang::enquo(time)
  event_enq <- rlang::enquo(event)

  fit <- survival::survfit(as.formula(
    rlang::expr(Surv(time = !! rlang::get_expr(time_enq),
                     event = !! rlang::get_expr(event_enq),
                     type = type)
                ~ 1)),
    data = .data)

  summary(fit, times = c(0, unique(fit$time)), extend = extend) %>%
    .[c("n", "time", "n.risk", "n.event", "surv", "lower", "upper")] %>%
    tibble::as_tibble() %>%
    mutate(cum_inc = 1 - surv) %>%
    janitor::clean_names()

}



