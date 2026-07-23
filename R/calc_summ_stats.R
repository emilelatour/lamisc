#' @title
#' Calculate summary / descriptive statistics
#'
#' @description
#' Provides brief overview statistics for selected variables of a `tbl_df`:
#' number of observations (n), complete observations (complete), missing
#' observations (missing); mean, standard deviation (sd), minimum value (p0),
#' 25th/50th/75th percentiles (p25, p50, p75), maximum value (p100), range, and
#' coefficient of variation (CV).
#'
#' `calc_summ_stats()` summarises existing columns. `calc_summ_stats_t()`
#' derives variables on the fly (via a `transmute()` step) and then summarises
#' them.
#'
#' When `.data` is grouped and `overall = TRUE`, an overall (ungrouped) summary
#' is added above the group-level rows.
#'
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr any_of
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr group_by
#' @importFrom dplyr group_vars
#' @importFrom dplyr mutate
#' @importFrom dplyr pick
#' @importFrom dplyr summarise
#' @importFrom dplyr transmute
#' @importFrom dplyr ungroup
#' @importFrom forcats fct_inorder
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom stats quantile
#' @importFrom stats sd
#'
#' @param .data A tbl.
#' @param ... Variables to summarise. For `calc_summ_stats_t()`, name-value
#'   pairs defining the variables to derive and then summarise.
#' @param overall Logical. When `.data` is grouped, should an overall summary be
#'   added on top of the group-level rows? Silently ignored when `.data` is
#'   ungrouped. Defaults to `FALSE`.
#' @param overall_label Character scalar placed in the grouping columns for the
#'   overall rows. Defaults to `"All"`. Errors if the label is already a value
#'   in one of the grouping columns.
#'
#' @details
#' When `overall = TRUE` and `.data` is grouped, the grouping columns come back
#' as **factors**, with `overall_label` as the first level. That is what floats
#' the overall rows to the top and keeps the remaining group order intact. Watch
#' for this if you join the result against something else downstream, since a
#' factor-to-character join will warn. Ungrouped calls, and grouped calls with
#' `overall = FALSE`, leave the grouping columns untouched.
#'
#' With more than one grouping variable, you get a single set of overall rows
#' with `overall_label` repeated across every grouping column. Full marginals
#' (each grouping variable summarised on its own) are not produced.
#'
#' Because `overall` and `overall_label` follow `...`, a derived variable in
#' `calc_summ_stats_t()` cannot be named `overall` or `overall_label`.
#'
#' Only numeric variables are supported.
#'
#' @return A tbl.
#'
#' @rdname calc_summ_stats
#' @export
#'
#' @examples
#' library(dplyr)  # the starwars data set lives here
#'
#' # Descriptive stats for height and mass
#' starwars %>%
#'   calc_summ_stats(height, mass)
#'
#' # Grouped by gender
#' starwars %>%
#'   group_by(gender) %>%
#'   calc_summ_stats(height, mass)
#'
#' # ... with an overall summary on top
#' starwars %>%
#'   group_by(gender) %>%
#'   calc_summ_stats(height, mass, overall = TRUE)
#'
#' # A different label for the overall rows
#' starwars %>%
#'   group_by(gender) %>%
#'   calc_summ_stats(height, mass, overall = TRUE, overall_label = "Total")
#'
#' # Multiple grouping variables: one set of overall rows, label in every
#' # grouping column
#' starwars %>%
#'   group_by(sex, gender) %>%
#'   calc_summ_stats(height, overall = TRUE)
#'
#' # Derive variables within the function, then summarise
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
#' # ... with an overall summary on top
#' starwars %>%
#'   group_by(gender) %>%
#'   calc_summ_stats_t(
#'     heightm = height / 100,
#'     bmi = mass / heightm^2,
#'     overall = TRUE
#'   )
#'
#' # Numeric variables only; factors and characters are not supported
#' # starwars %>%
#' #   calc_summ_stats(height, mass, gender)
calc_summ_stats <- function(.data, ..., overall = FALSE, overall_label = "All") {

  grp_vars <- dplyr::group_vars(.data)
  res <- summ_stats_worker(.data, ...)

  if (!isTRUE(overall) || length(grp_vars) == 0L) {
    return(res)
  }

  add_overall(res_grp = res,
              res_all = summ_stats_worker(dplyr::ungroup(.data), ...),
              grp_vars = grp_vars,
              label = overall_label)
}


#' @rdname calc_summ_stats
#' @export
calc_summ_stats_t <- function(.data, ..., overall = FALSE, overall_label = "All") {

  grp_vars <- dplyr::group_vars(.data)
  res <- summ_stats_worker_t(.data, ...)

  if (!isTRUE(overall) || length(grp_vars) == 0L) {
    return(res)
  }

  add_overall(res_grp = res,
              res_all = summ_stats_worker_t(dplyr::ungroup(.data), ...),
              grp_vars = grp_vars,
              label = overall_label)
}


#### Internal workers --------------------------------

#' Pivot and summarise existing columns
#'
#' @param .data A tbl.
#' @param ... Variables to summarise.
#'
#' @return A tbl.
#'
#' @noRd
summ_stats_worker <- function(.data, ...) {

  .data %>%
    tidyr::pivot_longer(data = .,
                        cols = c(...,
                                 -dplyr::any_of(dplyr::group_vars(.))),
                        names_to = "variable",
                        values_to = "value",
                        names_transform = list(variable = forcats::fct_inorder)) %>%
    summ_stats_tail()
}


#' Derive, pivot, and summarise
#'
#' @param .data A tbl.
#' @param ... Name-value pairs of variables to derive.
#'
#' @return A tbl.
#'
#' @noRd
summ_stats_worker_t <- function(.data, ...) {

  .data %>%
    dplyr::transmute(...) %>%
    tidyr::pivot_longer(data = .,
                        cols = c(dplyr::everything(),
                                 -dplyr::any_of(dplyr::group_vars(.))),
                        names_to = "variable",
                        values_to = "value",
                        names_transform = list(variable = forcats::fct_inorder)) %>%
    summ_stats_tail()
}


#' Shared summarise step for the two workers
#'
#' @param .data A long tbl with `variable` and `value` columns.
#'
#' @return A tbl.
#'
#' @noRd
summ_stats_tail <- function(.data) {

  .data %>%
    dplyr::group_by(.data$variable,
                    .add = TRUE) %>%
    dplyr::summarise(dplyr::across(.cols = "value",
                                   .fns = summary_functions,
                                   .names = "{.fn}"),
                     .groups = "drop") %>%
    dplyr::mutate(range = .data$p100 - .data$p0,
                  CV = 100 * .data$sd / .data$mean) %>%
    dplyr::mutate(variable = as.character(.data$variable))
}


#' Bind an overall summary on top of a grouped summary
#'
#' Grouping columns are converted to factors with `label` as the first level so
#' that the overall rows sort to the top and the remaining group order is
#' preserved.
#'
#' @param res_grp Grouped summary.
#' @param res_all Overall (ungrouped) summary.
#' @param grp_vars Character vector of grouping variable names.
#' @param label Character scalar for the overall rows.
#'
#' @return A tbl.
#'
#' @noRd
add_overall <- function(res_grp, res_all, grp_vars, label) {

  if (!is.character(label) || length(label) != 1L || is.na(label)) {
    stop("`overall_label` must be a single non-missing character string.",
         call. = FALSE)
  }

  for (g in grp_vars) {

    lv <- if (is.factor(res_grp[[g]])) {
      levels(res_grp[[g]])
    } else {
      unique(as.character(res_grp[[g]]))
    }

    lv <- setdiff(lv, NA_character_)

    if (label %in% lv) {
      stop("`overall_label` (\"", label, "\") is already a value in `", g,
           "`. Choose a different label.",
           call. = FALSE)
    }

    lv <- c(label, lv)

    res_grp[[g]] <- factor(as.character(res_grp[[g]]), levels = lv)
    res_all[[g]] <- factor(label, levels = lv)
  }

  res_all <- res_all[names(res_grp)]

  dplyr::bind_rows(res_all, res_grp) %>%
    dplyr::arrange(dplyr::pick(dplyr::all_of(grp_vars)))
}


#### Function to calc summary stats --------------------------------

summary_functions <- list(
  n =        ~ length(.),
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
