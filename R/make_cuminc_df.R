
#' @title
#' Calculate data frame of cumulative incidence.
#'
#' @description
#' Allows for easy grouping by catogories to get survival data from a Kaplan
#' Meier object as well as the cumulative incidence. TODO -- Confidence interval
#' for cumulative incidence.
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
#' @param ... Unquoted names of variables to group by. Prefer factor or
#'   character classes.
#' @param type character string specifying the type of censoring. Possible
#'   values are "right", "left", "counting", "interval", "interval2" or
#'   "mstate".
#' @param extend logical value: if TRUE, prints information for all specified
#'   times, even if there are no subjects left at the end of the specified
#'   times. This is only valid if the times argument is present.
#'
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom janitor clean_names
#' @importFrom purrr map
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom rlang expr
#' @importFrom rlang get_expr
#' @importFrom stats as.formula
#' @importFrom survival survfit
#' @importFrom tibble as_tibble
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#'
#' @return A tibble
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(survival)
#'
#' #### Data for example --------------------------------
#' # View the preloaded data
#' dplyr::glimpse(aml)
#'
#' ## Create a variable for smoke ----------------
#' df <- aml %>%
#'   mutate(smoke = sample(c("Yes", "No"), size = 23, replace = TRUE)) %>%
#'   tibble::as_tibble()
#' df
#'
#' #### Example #1 --------------------------------
#'
#' make_cuminc_df(data = df,
#'                time = time,
#'                event = status,
#'                smoke)
#'
#' #### Example #2 --------------------------------
#'
#' make_cuminc_df(data = df,
#'                time = time,
#'                event = status,
#'                x, smoke)


make_cuminc_df <- function(data, time, event, ...,
                           type = "right", extend = FALSE) {

  time <- rlang::enquo(time)
  event <- rlang::enquo(event)
  group_vars <- rlang::enquos(...)

  data %>%
    group_by(!!! group_vars) %>%
    tidyr::nest() %>%
    mutate(res = purrr::map(.x = data,
                            .f = ~ .calc_surv(data = .x,
                                              time = !! time,
                                              event = !! event,
                                              type = type,
                                              extend = extend))) %>%
    tidyr::unnest(res)


}
