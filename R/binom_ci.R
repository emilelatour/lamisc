
#' Compute Binomial Confidence Intervals
#'
#' A wrapper around `DescTools::BinomCI` to calculate confidence intervals for binomial proportions.
#' This function formats the output into a tibble and optionally formats percentages.
#'
#' @param x Numeric. The number of successes.
#' @param n Numeric. The number of trials.
#' @param conf_level Numeric. Confidence level for the interval, defaults to 0.95.
#' @param sides Character. Specifies the side of the confidence interval. Must be one of
#'   `"two.sided"` (default), `"left"`, or `"right"`. Only the first value is used if multiple are provided.
#' @param method Character. The method used to compute the confidence interval. This can be one of
#'   `"wilson"` (default), `"wald"`, `"waldcc"`, `"agresti-coull"`, `"jeffreys"`, `"modified wilson"`,
#'   `"wilsoncc"`, `"modified jeffreys"`, `"clopper-pearson"`, `"arcsine"`, `"logit"`, `"witting"`,
#'   `"pratt"`, `"midp"`, `"lik"`, or `"blaker"`. Abbreviations are accepted. Only the first value is used
#'   if multiple are provided.
#' @param rand Numeric. Seed for the random number generator. Used for certain methods.
#' @param tol Numeric. Tolerance for the `"blaker"` method.
#' @param std_est Logical. If `TRUE` (default), returns the standard point estimator for the proportion
#'   (x/n). If `FALSE`, returns the method-specific point estimate.
#' @param formatted Logical. If `TRUE`, formats the confidence intervals and point estimate as percentages.
#'   Defaults to `FALSE`.
#' @param accuracy Numeric. The rounding accuracy for formatted percentages. Defaults to 0.1.
#'
#' @importFrom DescTools BinomCI
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom janitor clean_names
#' @importFrom tibble as_tibble
#'
#' @return A tibble with the following columns:
#'   - `percentage`: The point estimate of the proportion.
#'   - `lower_ci`: The lower bound of the confidence interval.
#'   - `upper_ci`: The upper bound of the confidence interval.
#'   If `formatted = TRUE`, these values are formatted as percentages.
#'
#' @examples
#' # Compute a Wilson confidence interval
#' binom_ci(x = 25, n = 100, method = "wilson")
#'
#' # Compute a Clopper-Pearson confidence interval, formatted as percentages
#' binom_ci(x = 25, n = 100, method = "clopper-pearson", formatted = TRUE)
#'
#' @export
binom_ci <- function(x, n,
                     conf_level = 0.95,
                     sides = c("two.sided", "left", "right"),
                     method = c("wilson", "wald", "waldcc", "agresti-coull", "jeffreys",
                                "modified wilson", "wilsoncc","modified jeffreys",
                                "clopper-pearson", "arcsine", "logit", "witting", "pratt",
                                "midp", "lik", "blaker"),
                     rand = 123,
                     tol = 1e-05,
                     std_est = TRUE,
                     formatted = FALSE,
                     accuracy = 0.1) {

  # Fix no visible binding for global variable
  est <- lwr_ci <- upr_ci <- NULL

  res <- DescTools::BinomCI(x = x,
                            n = n,
                            conf.level = conf_level,
                            sides = sides[1],
                            method = method[1],
                            rand = rand,
                            tol = tol,
                            std_est = std_est) |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::rename(percentage = est,
                  lower_ci = lwr_ci,
                  upper_ci = upr_ci)

  if (formatted) {
    res <- res |>
      mutate(dplyr::across(.cols = dplyr::everything(),
                           .fns = ~ lamisc::fmt_pct(x = .,
                                                    accuracy = accuracy)))
  }

  return(res)

}
