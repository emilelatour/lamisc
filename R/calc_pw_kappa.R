#' @title
#' Calculate pairwiese agreement statistics for multiple raters
#'
#' @description
#' When working with multiple-raters, it can be helpful to look at pairwise
#' agreement for all raters. The goal of this function is to automate some of
#' the steps incvolved to calculate statistics for each pair and summarize them
#' nicely in a table or a data frame.
#'
#' Currently, pairwise kappa and proportion of obeserved agreement are the only
#' statistics available.
#'
#' @param data A data frame or tibble
#' @param ... Variable (column) names
#' @param type Character; "unweighted" or "weighted" kappa
#'
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @import broom
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom janitor clean_names
#' @importFrom irr agree
#' @importFrom psych cohen.kappa
#' @importFrom rlang .data
#'
#' @return A list
#' @export
#'
#' @examples
#' diagnostic_df <- data.frame(stringsAsFactors = FALSE,
#'   id = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L,
#'          15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L,
#'          24L, 25L, 26L, 27L, 28L, 29L, 30L),
#'   rater_1 = c("Yes", "No", "No", "No", "No", "No", "No", "No", "No", "No",
#'               "No", "No", "No", "No", "No", "No", "No",
#'               "No", "No", "No", "No", "Yes", "No", "No", "No",
#'               "No", "No", "No", "No", "No"),
#'   rater_2 = c("Yes", "No", "No", "No", "No", "No", "No", "No", "No", "No",
#'               "Yes", "No", "No", "Yes", "No", "No", "No",
#'               "No", "No", "No", "No", "Yes", "No", "No",
#'               "Yes", "No", "No", "No", "No", "No"),
#'   rater_3 = c("Yes", "No", "No", "No", "No", "No", "No", "No", "Yes", "No",
#'               "Yes", "Yes", "No", "Yes", "Yes", "No",
#'               "No", "No", "Yes", "No", "No", "Yes", "Yes",
#'               "Yes", "Yes", "No", "No", "Yes", "No", "No"),
#'   rater_4 = c("Yes", "No", "No", "No", "Yes", "No", "No", "No", "Yes", "No",
#'               "Yes", "Yes", "No", "Yes", "Yes", "No",
#'               "Yes", "No", "Yes", "No", "No", "Yes", "No",
#'               "Yes", "Yes", "No", "No", "Yes", "No", "No"),
#'   rater_5 = c("Yes", "No", "No", "No", "Yes", "No", "No", "No", "Yes", "No",
#'               "Yes", "Yes", "No", "Yes", "Yes", "No",
#'               "No", "No", "Yes", "No", "No", "Yes", "No", "Yes",
#'               "Yes", "No", "No", "Yes", "No", "No"),
#'   rater_6 = c("Yes", "No", "No", "No", "Yes", "No", "No", "Yes", "Yes",
#'               "No", "Yes", "Yes", "No", "Yes", "No", "No",
#'               "No", "No", "Yes", "No", "No", "No", "No",
#'               "Yes", "No", "Yes", "No", "Yes", "No", "No")
#' )
#'
#'
#' foo <- calc_pw_kappa(data = diagnostic_df,
#'                      rater_1:rater_6)
#' names(foo)
#' foo$k_table
#' foo$k_results
#' foo$k_min_max
#' foo$po_table
#' foo$po_results
#' foo$po_min_max
#'


calc_pw_kappa <- function(data, ..., type = "unweighted") {

  vars <- rlang::enquos(...)

  #### Check number of ratings --------------------------------

  check <- data %>%
    dplyr::select(!!! vars) %>%
    dplyr::pull(.) %>%
    unique(.)

  if (length(check) > 2) stop("Only 2 ratings are allowed.")

  #### Helper functions --------------------------------

  ## get_table ---------------

  get_table <- function(data, x, y) {
    table(data[[x]], data[[y]])
  }

  ## get_kappa ---------------

  get_kappa <- function(table) {
    invisible(
      suppressWarnings(
        suppressMessages(
          broom::tidy(psych::cohen.kappa(x = table)) %>%
            # dplyr::filter(type == type) %>%
            janitor::clean_names(.) #%>%
          # dplyr::select(estimate, conf_low, conf_high)
        )
      )
    )
  }

  ## get_agree ----------------

  get_agree <- function(data, x, y) {
    # x <- rlang::enquo(x)
    # y <- rlang::enquo(y)
    data %>%
      # dplyr::select(!! x, !! y) %>%
      dplyr::select(x, y) %>%
      irr::agree(.)

  }


  #### Pairwise kappa --------------------------------

  pw_k_results <- data %>%
    dplyr::select(!!! vars) %>%
    names() %>%
    tidyr::crossing(x = ., y = .) %>%
    mutate(table = purrr::map2(.x = .data$x,
                               .y = .data$y,
                               .f = ~ get_table(data = data, x = .x, y = .y)),
           kap = purrr::map(.x = .data$table,
                            .f = ~ get_kappa(table = .x))) %>%
    tidyr::unnest(data = ., .data$kap) %>%
    # dplyr::filter(type == type) %>%
    mutate(combo =
             paste0(lamisc::roundr(.data$estimate, d = 2, as_text = TRUE),
                    " (",
                    lamisc::roundr(.data$conf_low, d = 2, as_text = TRUE),
                    " to ",
                    lamisc::roundr(.data$conf_high, d = 2, as_text = TRUE),
                    ")"))  %>%
    dplyr::rename(
      # new = old
      "lower_ci" = "conf_low",
      "upper_ci" = "conf_high"
    ) # %>%
  # dplyr::select(x, y, combo) %>%
  # tidyr::spread(key = y, value = combo)


  ## Make table of kappas ---------------

  if (type == "unweighted") {
    pw_k_results <- pw_k_results %>%
      dplyr::filter(type == "unweighted")
  } else if (type == "weighted") {
    pw_k_results <- pw_k_results %>%
      dplyr::filter(type == "weighted")
  }


  pw_k_table <- pw_k_results %>%
    dplyr::select(.data$x, .data$y, .data$combo) %>%
    tidyr::spread(data = ., key = .data$y, value = .data$combo)


  ## Turn values above diagonal to "" ---------------
  pw_k_table[upper.tri(pw_k_table)] <- ""


  #### Get min/max kappa --------------------------------

  ranked_res <- pw_k_results %>%
    dplyr::filter(.data$estimate < 1.0) %>%
    mutate(rank = rank(.data$estimate,
                       ties.method = "first"))

  min_max_kappa <- dplyr::bind_rows(ranked_res %>%
                                      dplyr::slice(which.min(rank)),
                                    ranked_res %>%
                                      dplyr::slice(which.max(rank))
  )


  #### Observed agreement pairwise --------------------------------

  pw_po_results <- data %>%
    dplyr::select(!!! vars) %>%
    names() %>%
    tidyr::crossing(x = ., y = .) %>%
    mutate(table = purrr::map2(.x = .data$x,
                               .y = .data$y,
                               .f = ~ get_table(data = data, x = .x, y = .y)),
           n = purrr::map2(.x = .data$x,
                           .y = .data$y,
                           .f = ~ get_agree(data = data,
                                            x = .x,
                                            y = .y)$subjects),
           po = purrr::map2(.x = .data$x,
                            .y = .data$y,
                            .f = ~ get_agree(data = data,
                                             x = .x,
                                             y = .y)$value / 100)) %>%
    tidyr::unnest(data = ., .data$n, .data$po) %>%
    mutate(se_po = sqrt(.data$po * (1 - .data$po) / .data$n),
           lower_ci = .data$po + c(-1) * qnorm(1 - .05 / 2) * .data$se_po,
           upper_ci = .data$po + c(1) * qnorm(1 - .05 / 2) * .data$se_po) %>%
    mutate_at(.vars = vars(.data$po:.data$upper_ci),
              .funs = funs(lamisc::roundr(., d = 3))) %>%
    mutate(combo =
             paste0(lamisc::roundr(.data$po, d = 2, as_text = TRUE),
                    " (",
                    lamisc::roundr(.data$lower_ci, d = 2, as_text = TRUE),
                    " to ",
                    lamisc::roundr(.data$upper_ci, d = 2, as_text = TRUE),
                    ")"))

  ## Make table of observed agreement ---------------

  pw_po_table <- pw_po_results %>%
    dplyr::select(.data$x, .data$y, .data$combo) %>%
    tidyr::spread(data = ., key = .data$y, value = .data$combo)


  ## Turn values above diagonal to "" ---------------
  pw_po_table[upper.tri(pw_po_table)] <- ""


  #### Get min/max po --------------------------------

  ranked_po <- pw_po_results %>%
    dplyr::filter(.data$po < 1.0) %>%
    mutate(rank = rank(.data$po,
                       ties.method = "first"))

  min_max_po <- dplyr::bind_rows(ranked_po %>%
                                   dplyr::slice(which.min(rank)),
                                 ranked_po %>%
                                   dplyr::slice(which.max(rank))
  )



  #### Return a list --------------------------------

  return(
    list(
      k_results = pw_k_results,
      k_table = pw_k_table,
      k_min_max = min_max_kappa,
      po_results = pw_po_results,
      po_table = pw_po_table,
      po_min_max = min_max_po
    ))


}
