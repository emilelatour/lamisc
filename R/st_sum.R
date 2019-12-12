#' @title
#' Summarize survival-time data
#'
#' @description
#' An R flavored version of the Stata command `stsum`. Presents summary
#' statistics: time at risk; incidence rate; number of subjects; and the 25th,
#' 50th, and 75th percentiles of survival time.
#'
#' @param data A data frame
#' @param time For right censored data, this is the follow up time. For interval
#'   data, the first argument is the starting time for the interval.
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
#' @param group Variable name (unquoted) to stratify on
#'
#' @references
#' https://www.stata.com/manuals13/ststsum.pdf
#'
#' @importFrom dplyr bind_cols
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr n
#' @importFrom dplyr rename
#' @importFrom dplyr summarise
#' @importFrom purrr pluck
#' @importFrom rlang enquo
#' @importFrom rlang expr
#' @importFrom rlang get_expr
#' @importFrom rlang quo_name
#' @importFrom survival Surv
#' @importFrom survival survfit
#' @importFrom tibble as_tibble
#' @importFrom tibble enframe
#' @importFrom tibble tibble
#' @importFrom tidyr spread
#'
#' @return
#' A list
#'
#' @export
#'
#' @examples
#' #### Example 1 --------------------------------
#'
#' library(dplyr)
#' library(tibble)
#' page_2 <- tibble::tribble(
#'   ~group, ~time, ~dead, ~st, ~d, ~t, ~t0,
#'   1,   143,     1,    1,   1, 143,    0,
#'   1,   164,     1,    1,   1, 164,    0,
#'   1,   188,     1,    1,   1, 188,    0,
#'   1,   188,     1,    1,   1, 188,    0,
#'   1,   190,     1,    1,   1, 190,    0,
#'   1,   192,     1,    1,   1, 192,    0,
#'   1,   206,     1,    1,   1, 206,    0,
#'   1,   209,     1,    1,   1, 209,    0,
#'   1,   213,     1,    1,   1, 213,    0,
#'   1,   216,     1,    1,   1, 216,    0,
#'   1,   220,     1,    1,   1, 220,    0,
#'   1,   227,     1,    1,   1, 227,    0,
#'   1,   230,     1,    1,   1, 230,    0,
#'   1,   234,     1,    1,   1, 234,    0,
#'   1,   246,     1,    1,   1, 246,    0,
#'   1,   265,     1,    1,   1, 265,    0,
#'   1,   304,     1,    1,   1, 304,    0,
#'   1,   216,     0,    1,   0, 216,    0,
#'   1,   244,     0,    1,   0, 244,    0,
#'   2,   142,     1,    1,   1, 142,    0,
#'   2,   156,     1,    1,   1, 156,    0,
#'   2,   163,     1,    1,   1, 163,    0,
#'   2,   198,     1,    1,   1, 198,    0,
#'   2,   205,     1,    1,   1, 205,    0,
#'   2,   232,     1,    1,   1, 232,    0,
#'   2,   232,     1,    1,   1, 232,    0,
#'   2,   233,     1,    1,   1, 233,    0,
#'   2,   233,     1,    1,   1, 233,    0,
#'   2,   233,     1,    1,   1, 233,    0,
#'   2,   233,     1,    1,   1, 233,    0,
#'   2,   239,     1,    1,   1, 239,    0,
#'   2,   240,     1,    1,   1, 240,    0,
#'   2,   261,     1,    1,   1, 261,    0,
#'   2,   280,     1,    1,   1, 280,    0,
#'   2,   280,     1,    1,   1, 280,    0,
#'   2,   296,     1,    1,   1, 296,    0,
#'   2,   296,     1,    1,   1, 296,    0,
#'   2,   323,     1,    1,   1, 323,    0,
#'   2,   204,     0,    1,   0, 204,    0,
#'   2,   344,     0,    1,   0, 344,    0
#' )
#'
#'
#' st_sum(data = page_2,
#'        time = time,
#'        event = dead,
#'        group = group)
#'
#' #### Example 2 --------------------------------
#'
#' sample_df <- tibble::tribble(
#'   ~exp, ~time, ~disease, ~st, ~d, ~t, ~t0,
#'      0,     1,        1,    1,   1,   1,    0,
#'      0,     1,        1,    1,   1,   1,    0,
#'      0,     1,        1,    1,   1,   1,    0,
#'      0,     1,        1,    1,   1,   1,    0,
#'      0,     1,        1,    1,   1,   1,    0,
#'      0,     1,        1,    1,   1,   1,    0,
#'      0,     1,        1,    1,   1,   1,    0,
#'      0,     1,        1,    1,   1,   1,    0,
#'      0,     2,        1,    1,   1,   2,    0,
#'      0,     2,        1,    1,   1,   2,    0,
#'      0,     2,        1,    1,   1,   2,    0,
#'      0,     2,        1,    1,   1,   2,    0,
#'      0,     2,        1,    1,   1,   2,    0,
#'      0,     2,        1,    1,   1,   2,    0,
#'      0,     2,        1,    1,   1,   2,    0,
#'      0,     2,        1,    1,   1,   2,    0,
#'      0,     2,        1,    1,   1,   2,    0,
#'      0,     2,        1,    1,   1,   2,    0,
#'      0,     2,        1,    1,   1,   2,    0,
#'      0,     3,        1,    1,   1,   3,    0,
#'      0,     3,        1,    1,   1,   3,    0,
#'      0,     4,        0,    1,   0,   4,    0,
#'      0,     5,        0,    1,   0,   5,    0,
#'      0,     5,        0,    1,   0,   5,    0,
#'      0,     5,        1,    1,   1,   5,    0,
#'      0,     6,        0,    1,   0,   6,    0,
#'      0,     7,        0,    1,   0,   7,    0,
#'      0,     7,        0,    1,   0,   7,    0,
#'      0,     8,        0,    1,   0,   8,    0,
#'      1,     1,        1,    1,   1,   1,    0,
#'      1,     1,        1,    1,   1,   1,    0,
#'      1,     1,        1,    1,   1,   1,    0,
#'      1,     1,        1,    1,   1,   1,    0,
#'      1,     1,        1,    1,   1,   1,    0,
#'      1,     1,        1,    1,   1,   1,    0,
#'      1,     1,        1,    1,   1,   1,    0,
#'      1,     2,        1,    1,   1,   2,    0,
#'      1,     2,        1,    1,   1,   2,    0,
#'      1,     2,        1,    1,   1,   2,    0,
#'      1,     2,        1,    1,   1,   2,    0,
#'      1,     2,        1,    1,   1,   2,    0,
#'      1,     2,        1,    1,   1,   2,    0,
#'      1,     2,        1,    1,   1,   2,    0,
#'      1,     2,        1,    1,   1,   2,    0,
#'      1,     3,        1,    1,   1,   3,    0,
#'      1,     3,        1,    1,   1,   3,    0,
#'      1,     4,        0,    1,   0,   4,    0,
#'      1,     4,        0,    1,   0,   4,    0,
#'      1,     4,        0,    1,   0,   4,    0,
#'      1,     4,        0,    1,   0,   4,    0,
#'      1,     4,        1,    1,   1,   4,    0,
#'      1,     5,        0,    1,   0,   5,    0,
#'      1,     5,        0,    1,   0,   5,    0,
#'      1,     6,        0,    1,   0,   6,    0,
#'      1,     6,        0,    1,   0,   6,    0,
#'      1,    10,        0,    1,   0,  10,    0,
#'      1,    12,        0,    1,   0,  12,    0
#'   )
#'
#'
#' st_sum(data = sample_df,
#'        time = time,
#'        event = disease,
#'        group = exp)
#'
#'
#' st_sum(data = sample_df,
#'        time = time,
#'        event = (disease == 1),
#'        group = exp)

st_sum <- function(data,
                   time,
                   event,
                   type = "right",
                   group) {

  time_enq <- rlang::enquo(time)
  event_enq <- rlang::enquo(event)
  group_enq <- rlang::enquo(group)


  #### Summary for total data --------------------------------

  # Just bind it instead of assigning objects

  total <- dplyr::bind_cols(

    # First column with label
    tibble::tibble(`strata` = "All") %>%
      mutate(strata = factor(strata)),

    # Summary stats
    data %>%
      dplyr::summarise(
        time_at_risk = sum(!! time_enq, na.rm = TRUE),
        crude_incidence =
          sum(!! event_enq, na.rm = TRUE) / time_at_risk,
        no_of_subjects = dplyr::n())
    ,

    # quantiles
    data %>%
      with(.,
           survival::survfit(as.formula(
             rlang::expr(survival::Surv(time = !! rlang::get_expr(time_enq),
                                        event = !! rlang::get_expr(event_enq),
                                        type = type)
                         ~ 1)))) %>%
      quantile(., probs = c(25, 50, 75) / 100) %>%
      purrr::pluck(.x = .,
                   "quantile") %>%
      # tibble::as_tibble() %>%
      tibble::enframe(name = NULL) %>%
      mutate(key = c("q_25", "q_50", "q_75")) %>%
      tidyr::spread(., key = key, value = value) %>%
      dplyr::mutate_all(.tbl = .,
                        .funs = list(~ as.integer(.)))

  )


  #### Summary by groups --------------------------------

  # Just bind it instead of assigning objects
  grouped <- dplyr::bind_cols(

    # Summary stats
    data %>%
      dplyr::group_by(!! group_enq) %>%
      dplyr::summarise(
        time_at_risk = sum(!! time_enq, na.rm = TRUE),
        crude_incidence =
          sum(!! event_enq, na.rm = TRUE) / time_at_risk,
        no_of_subjects = dplyr::n()) %>%
      dplyr::rename(`strata` = !! rlang::quo_name(group_enq))
    ,

    # quantiles
    # quantiles
    data %>%
      with(.,
           survival::survfit(as.formula(
             rlang::expr(survival::Surv(time = !! rlang::get_expr(time_enq),
                                        event = !! rlang::get_expr(event_enq),
                                        type = type)
                         ~ !! rlang::get_expr(group_enq))))) %>%
      quantile(., probs = c(25, 50, 75) / 100) %>%
      purrr::pluck(.x = .,
                   "quantile") %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(.tbl = .,
                        .funs = list(~ as.integer(.))) %>%
      dplyr::rename(
        # new = old
        q_25 = 1,
        q_50 = 2,
        q_75 = 3)
  )


  list(total = tibble::as_tibble(total),
       by_group = grouped)

}

