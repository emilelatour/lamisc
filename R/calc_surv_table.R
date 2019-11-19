#' @title
#' Calculate time-to-event estimates
#'
#' @description
#' Takes a `survfit` object and returns a tibble with median and confidence
#' intervals.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom janitor clean_names
#' @importFrom purrr pluck
#' @importFrom tibble as_tibble
#' @importFrom tibble enframe
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr spread
#'
#' @param fit `survfit` object
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
#' calc_surv_table(fit)
#'
#' fit <- survival::survfit(
#'   survival::Surv(time = time,
#'                  event = status) ~ 1,
#'   data = aml)
#'
#' calc_surv_table(fit)

calc_surv_table <- function(fit,
                            all_group_name = "All subjects") {


  if ("strata" %in% names(fit)) {

    fit %>%
      summary() %>%
      purrr::pluck(.,
                   "table") %>%
      data.frame() %>%
      janitor::clean_names() %>%
      tibble::rownames_to_column(.,
                                 var = "group") %>%
      mutate(group = gsub(".*=", "", group)) %>%
      dplyr::select(group,
                    n_start,
                    events,
                    median,
                    lower_ci = x0_95lcl,
                    upper_ci = x0_95ucl) %>%
      tibble::as_tibble()

  } else {

    fit %>%
      summary() %>%
      purrr::pluck(.,
                   "table") %>%
      tibble::enframe() %>%
      mutate(name = forcats::fct_inorder(name)) %>%
      tidyr::spread(.,
                    key = name,
                    value = value) %>%
      janitor::clean_names() %>%
      mutate(group = all_group_name) %>%
      dplyr::select(group,
                    n_start,
                    events,
                    median,
                    lower_ci = x0_95lcl,
                    upper_ci = x0_95ucl)
  }

}

