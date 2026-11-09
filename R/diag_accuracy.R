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
diag_accuracy <- function(tab,
                          test_dim   = 1,
                          positive   = "TRUE",
                          conf_level = 0.95,
                          method     = "wilson",
                          formatted  = FALSE,
                          accuracy   = 0.1) {

  # Fix no visible binding for global variable
  percentage <- estimate <- lower_ci <- upper_ci <- NULL

  # Check inputs
  if (!is.table(tab) && !is.matrix(tab)) {
    stop("`tab` must be a table or matrix, e.g. table(test = x, reference = y).",
         call. = FALSE)
  }

  if (!identical(dim(tab), c(2L, 2L))) {
    stop(paste("`tab` must be 2 x 2. Check the inputs for NA or extra levels,",
               "or a level with no observations (use factor() with both levels)."),
         call. = FALSE)
  }

  if (is.null(rownames(tab)) || is.null(colnames(tab))) {
    stop("`tab` must have row and column names.", call. = FALSE)
  }

  if (!test_dim %in% c(1, 2)) {
    stop("`test_dim` must be 1 (rows) or 2 (columns).", call. = FALSE)
  }

  valid_methods <- c("wilson", "wald", "waldcc", "agresti-coull", "jeffreys",
                     "modified wilson", "wilsoncc", "modified jeffreys",
                     "clopper-pearson", "arcsine", "logit", "witting", "pratt",
                     "midp", "lik", "blaker")

  if (!is.character(method) || length(method) != 1 ||
      !method %in% valid_methods) {
    stop(sprintf("`method` must be one of: %s.",
                 paste0('"', valid_methods, '"', collapse = ", ")),
         call. = FALSE)
  }

  # Index test in rows, reference standard in columns
  if (test_dim == 2) {
    tab <- t(tab)
  }

  positive <- as.character(positive)
  rn <- rownames(tab)
  cn <- colnames(tab)

  if (!positive %in% rn || !positive %in% cn) {
    stop(sprintf(
      "`positive` level '%s' not found in both dimensions (rows: %s; columns: %s).",
      positive, paste(rn, collapse = ", "), paste(cn, collapse = ", ")),
      call. = FALSE)
  }

  neg_r <- setdiff(rn, positive)
  neg_c <- setdiff(cn, positive)

  # Cell counts
  tp <- tab[positive, positive]
  fp <- tab[positive, neg_c]
  fn <- tab[neg_r, positive]
  tn <- tab[neg_r, neg_c]

  measures <- c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy", "Prevalence")
  num      <- c(tp,      tn,      tp,      tn,      tp + tn,  tp + fn)
  den      <- c(tp + fn, tn + fp, tp + fp, tn + fn, sum(tab), sum(tab))

  # Confidence intervals, NA when the denominator is 0
  ci <- purrr::map2(num, den, \(x, n) {
    if (n == 0) {
      return(tibble::tibble(estimate = NA_real_,
                            lower_ci = NA_real_,
                            upper_ci = NA_real_))
    }
    binom_ci(x          = x,
             n          = n,
             conf_level = conf_level,
             method     = method,
             formatted  = FALSE) |>
      dplyr::rename(estimate = percentage)
  }) |>
    dplyr::bind_rows()

  res <- tibble::tibble(measure = measures,
                        n       = as.integer(num),
                        total   = as.integer(den)) |>
    dplyr::bind_cols(ci)

  if (formatted) {
    res <- res |>
      dplyr::mutate(dplyr::across(.cols = c(estimate, lower_ci, upper_ci),
                                  .fns  = \(x) lamisc::fmt_pct(x        = x,
                                                               accuracy = accuracy)))
  }

  attr(res, "counts")    <- c(tp = unname(tp), fp = unname(fp),
                              fn = unname(fn), tn = unname(tn))
  attr(res, "ci_method") <- method

  return(res)

}
