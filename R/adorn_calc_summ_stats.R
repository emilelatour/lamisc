#' Add and Calculate Summary Statistics Columns to a Table
#'
#' This function formats numeric columns, dynamically adds summary statistics
#' based on a list of formulas, and returns the formatted table.
#'
#' @param tab A data frame or tibble containing the data.
#' @param accuracy Numeric. The desired rounding accuracy for summary statistics (default: 0.1).
#' @param scale Numeric. A scaling factor to apply to numeric values (default: 1.0).
#' @param prefix Character. A string to prepend to formatted values (default: "").
#' @param suffix Character. A string to append to formatted values (default: "").
#' @param big_mark Character. The thousands separator (default: "").
#' @param decimal_mark Character. The decimal separator (default: ".").
#' @param style_positive Character. Style for positive numbers. Options: "none", "plus" (default: "none").
#' @param style_negative Character. Style for negative numbers. Options: "hyphen", "minus", "parens" (default: "hyphen").
#' @param scale_cut A function to apply to scaled values for trimming (default: NULL).
#' @param trim Logical. Whether to trim whitespace from formatted numbers (default: TRUE).
#' @param form_list A list of formulas for dynamically adding summary statistic columns (default: list of common summary statistics).
#'
#' @return A tibble with formatted and dynamically added columns for summary statistics.
#'
#' @importFrom dplyr mutate across select all_of
#' @importFrom scales number
#' @importFrom purrr map
#' @importFrom glue glue
#' @importFrom stringr str_remove_all str_replace
#' @importFrom janitor make_clean_names
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
#'   ) |>
#'   adorn_calc_summ_stats()
#'
#' # Grouped by gender
#' starwars %>%
#'   group_by(gender) %>%
#'   calc_summ_stats(
#'     height, mass
#'   ) |>
#'   adorn_calc_summ_stats()
#'
#' # Derive variables within function then summarise
#' starwars %>%
#'   calc_summ_stats_t(
#'     heightm = height / 100,
#'     bmi = mass / heightm^2
#'   ) |>
#'   adorn_calc_summ_stats()
#'
#' # Grouped by gender
#' starwars %>%
#'   group_by(gender) %>%
#'   calc_summ_stats_t(
#'     heightm = height / 100,
#'     bmi = mass / heightm^2
#'   ) |>
#'   adorn_calc_summ_stats(form_list = list("{Min}, {Max}"))
adorn_calc_summ_stats <- function(tab,
                                  accuracy = 0.1,
                                  scale = 1.0,
                                  prefix = "",
                                  suffix = "",
                                  big_mark = "",
                                  decimal_mark = ".",
                                  style_positive = c("none", "plus"),
                                  style_negative = c("hyphen", "minus", "parens"),
                                  scale_cut = NULL,
                                  trim = TRUE,
                                  form_list = list("{mean} ({sd})",
                                                   "{p50} [{p25} to {p75}]",
                                                   "{p0} to {p100}")) {
  # Function body here...
}


adorn_calc_summ_stats <- function(tab,
                                  accuracy = 0.1,
                                  scale = 1.0,
                                  prefix = "",
                                  suffix = "",
                                  big_mark = "",
                                  decimal_mark = ".",
                                  style_positive = c("none", "plus"),
                                  style_negative = c("hyphen", "minus", "parens"),
                                  scale_cut = NULL,
                                  trim = TRUE,
                                  form_list = list("{mean} ({sd})",
                                                   "{p50} [{p25} to {p75}]",
                                                   "{p0} to {p100}")) {

  # Silence warning Undefined global functions or variables
  CV <- NULL

  # Format numbers
  tab <- tab |>
    mutate(dplyr::across(.cols = c(n:missing),
                         .fns = ~ scales::number(x = .,
                                                 accuracy = 1.0,
                                                 scale = 1,
                                                 prefix = "",
                                                 suffix = "",
                                                 big.mark = big_mark,
                                                 decimal.mark = decimal_mark,
                                                 style_positive = "none",
                                                 style_negative = "hyphen",
                                                 scale_cut = scale_cut,
                                                 trim = trim))) |>
    mutate(dplyr::across(.cols = c(mean:CV),
                         .fns = ~  scales::number(x = .,
                                                  accuracy = accuracy,
                                                  scale = scale,
                                                  prefix = prefix,
                                                  suffix = suffix,
                                                  big.mark = big_mark,
                                                  decimal.mark = decimal_mark,
                                                  style_positive = style_positive[1],
                                                  style_negative = style_negative[1],
                                                  scale_cut = scale_cut,
                                                  trim = trim)))



  # Fix any stray text
  form_list <- form_list |>
    purrr::map(.f = ~ fix_glue_formulas(.x))


  # Variable names
  form_vars <- form_list |>
    purrr::map(.f = ~ stringr::str_remove_all(.x, pattern = "\\{")) |>
    purrr::map(.f = ~ stringr::str_remove_all(.x, pattern = "\\}")) |>
    purrr::map(.f = ~ janitor::make_clean_names(.x)) |>
    purrr::map(.f = ~ stringr::str_replace(.x,
                                           pattern = "p50",
                                           replacement = "median")) |>
    purrr::map(.f = ~ stringr::str_replace(.x,
                                           pattern = "p25_to_p75",
                                           replacement = "iqr")) |>
    purrr::map(.f = ~ stringr::str_replace(.x,
                                           pattern = "p0_to_p100",
                                           replacement = "range")) |>
    purrr::map(.f = ~ stringr::str_replace(.x,
                                           pattern = "p25_p75",
                                           replacement = "iqr")) |>
    purrr::map(.f = ~ stringr::str_replace(.x,
                                           pattern = "p0_p100",
                                           replacement = "range"))



  # Add columns dynamically using form_list and form_vars
  for (i in seq_along(form_vars)) {
    var_name <- form_vars[[i]] # Get the variable name
    formula <- form_list[[i]]  # Get the corresponding glue formula

    # Add the column to the data frame
    tab <- tab %>%
      mutate(!!var_name := glue::glue(formula))
  }


  # Select columns to return
  tab <- tab |>
    dplyr::select(1:missing,
                  dplyr::all_of(unlist(form_vars)))

  return(tab)

}


## Fix formula ----------------


fix_glue_formulas <- function(glue_formula) {

  glue_formula = stringr::str_replace_all(string = glue_formula,
                                          pattern = "mean|Mean|avg|average",
                                          replacement = "mean")

  glue_formula = stringr::str_replace_all(string = glue_formula,
                                          pattern = "sd|SD",
                                          replacement = "sd")


  glue_formula = stringr::str_replace_all(string = glue_formula,
                                          pattern = "median|Median|med|med",
                                          replacement = "p50")

  glue_formula = stringr::str_replace_all(string = glue_formula,
                                          pattern = "min|Min|minimum|Minimum",
                                          replacement = "p0")

  glue_formula = stringr::str_replace_all(string = glue_formula,
                                          pattern = "max|Max|maximum|Maximum",
                                          replacement = "p100")

  glue_formula = stringr::str_replace_all(string = glue_formula,
                                          pattern = "\\{iqr\\}",
                                          replacement = "{p25} to {p75}")

  glue_formula = stringr::str_replace_all(string = glue_formula,
                                          pattern = "\\{IQR\\}",
                                          replacement = "{p25} to {p75}")

  glue_formula = stringr::str_replace_all(string = glue_formula,
                                          pattern = "\\{range\\}",
                                          replacement = "{p0} to {p100}")

}




