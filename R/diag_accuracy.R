#' Diagnostic accuracy from a 2 x 2 table
#'
#' Computes sensitivity, specificity, positive and negative predictive values
#' (PPV, NPV), overall accuracy, and prevalence for an index test compared with
#' a reference standard, each with a confidence interval from [binom_ci()].
#'
#' The table is oriented with the index test in rows and the reference
#' standard in columns. Use `test_dim = 2` if yours is the other way around.
#' The level named by `positive` must appear in both dimensions; the other
#' level is treated as negative.
#'
#' Each measure is a simple binomial proportion, so the intervals assume the
#' table comes from a census or a simple random sample of the population of
#' interest. They are not appropriate for stratified or weighted samples.
#' PPV, NPV, and accuracy depend on prevalence and describe the population the
#' table came from.
#'
#' @param tab A 2 x 2 `table` or `matrix` of counts with dimnames, e.g. from
#'   `table(test = x, reference = y)`.
#' @param test_dim Integer. Which dimension holds the index test: `1` (rows,
#'   default) or `2` (columns).
#' @param positive Character. The level that means "positive" in both
#'   dimensions. Defaults to `"TRUE"`, which suits tables made from logical
#'   vectors. Use e.g. `"Yes"` or `"1"` for other codings.
#' @param conf_level Numeric. Confidence level for the intervals. Defaults to
#'   0.95.
#' @param method Character. Confidence interval method, passed to
#'   [binom_ci()]. Defaults to `"wilson"`. Use `"clopper-pearson"` for exact
#'   intervals.
#' @param formatted Logical. If `TRUE`, `estimate`, `lower_ci`, and `upper_ci`
#'   are formatted as percentages. Defaults to `FALSE`.
#' @param accuracy Numeric. Rounding accuracy for formatted percentages.
#'   Defaults to 0.1.
#'
#' @importFrom dplyr across
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom purrr map2
#' @importFrom tibble tibble
#'
#' @return A tibble with one row per measure and columns:
#'   - `measure`: Sensitivity, Specificity, PPV, NPV, Accuracy, Prevalence.
#'   - `n`: The numerator count.
#'   - `total`: The denominator count.
#'   - `estimate`, `lower_ci`, `upper_ci`: The proportion and its confidence
#'     interval (`NA` when `total` is 0).
#'
#'   The cell counts are attached as attribute `"counts"`
#'   (`tp`, `fp`, `fn`, `tn`), and the interval method as `"ci_method"`.
#'
#' @seealso [binom_ci()]
#'
#' @examples
#' # Index test in rows, reference standard in columns
#' test      <- c(rep(TRUE, 225), rep(FALSE, 1047))
#' reference <- c(rep(TRUE, 205), rep(FALSE, 20), rep(TRUE, 240), rep(FALSE, 807))
#'
#' tab <- table(test = test, reference = reference)
#' tab
#'
#' res <- diag_accuracy(tab)
#' res
#' attr(res, "counts")
#'
#' # Exact intervals, formatted as percentages
#' diag_accuracy(tab, method = "clopper-pearson", formatted = TRUE)
#'
#' # Reference standard in rows instead
#' diag_accuracy(t(tab), test_dim = 2)
#'
#' # Yes/No coding
#' tab_yn <- table(test      = ifelse(test, "Yes", "No"),
#'                 reference = ifelse(reference, "Yes", "No"))
#' diag_accuracy(tab_yn, positive = "Yes")
#'
#' @export
