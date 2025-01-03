#' Perform a Binomial Test Using a Categorical Variable
#'
#' @param data A tibble or data frame containing the data.
#' @param var The categorical variable to test.
#' @param success_value (Optional) The value considered as "success" in the binomial test.
#' @param p_null The hypothesized probability of success under the null hypothesis (default = 0.5).
#' @param alternative The alternative hypothesis: "two.sided", "less", or "greater".
#' @param conf_level The confidence level for the interval (default = 0.95).
#'
#' @return A tibble containing the binomial test results, including:
#'   - `x`: Number of successes.
#'   - `n`: Total trials.
#'   - `estimate`: Observed success proportion.
#'   - `lower_ci`: Lower confidence interval.
#'   - `upper_ci`: Upper confidence interval.
#'   - `p_value`: p-value for the test.
#'   - `p_null`: Null hypothesis proportion.
#'   - `success_value`: The value considered as success.
#'
#' @importFrom janitor tabyl adorn_totals
#' @importFrom dplyr filter pull mutate select slice
#' @importFrom broom tidy
#' @importFrom stats binom.test
#' @importFrom janitor clean_names
#'
#' @export
#'
#' @examples
#' library(tibble)
#'
#' mice_data <- tibble(
#'   mouse_id = 1:160,
#'   sex = c(rep("Male", 95), rep("Female", 65)),
#'   cancer_status = rep("Cancer", 160)
#' )
#'
#' calc_binomial_test(data = mice_data, var = sex)
calc_binomial_test <- function(data,
                               var,
                               success_value = NULL,
                               p_null = 0.5,
                               alternative = c("two.sided", "less", "greater"),
                               conf_level = 0.95) {

  # Silence warning no visible binding for global variable
  conf_high <- conf_low <- estimate <- p_value <- x <- NULL

  # Summarize the data
  summarized_data <- data |>
    janitor::tabyl({{ var }}) %>%
    adorn_totals("row")

  # Ensure success value
  if (is.null(success_value)) {
    success_value <- summarized_data |>
      dplyr::slice(1) |>
      dplyr::pull(1)
  }

  # Observed successes and total trials
  successes <- summarized_data |>
    dplyr::filter(summarized_data[[1]] == success_value) |>
    dplyr::pull(n)

  trials <- summarized_data |>
    dplyr::filter(summarized_data[[1]] == "Total") |>
    dplyr::pull(n)

  # Perform the binomial test
  results <- binom.test(x = successes,
                        n = trials,
                        p = p_null,
                        alternative = alternative,
                        conf.level = conf_level) |>
    broom::tidy() |>
    janitor::clean_names() |>
    mutate(x = successes,
           n = trials,
           p_null = p_null,
           success_value = success_value) |>
    dplyr::select(x,
                  n,
                  estimate,
                  lower_ci = conf_low,
                  upper_ci = conf_high,
                  p_value,
                  p_null,
                  success_value)

  return(results)
}
