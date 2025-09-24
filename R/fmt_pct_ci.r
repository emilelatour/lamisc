#' Format percentages with confidence intervals
#'
#' @description
#' A convenience wrapper around [lamisc::fmt_pct()] and [glue::glue()] to format
#' point estimates and confidence intervals consistently for text and tables.
#' By default, the point estimate is shown with a percent sign and the bounds
#' are shown without, producing output like `"45% (95% CI: 41–49)"`.
#'
#' @param p Numeric vector of point estimates (typically proportions).
#' @param lower_ci Numeric vector of lower confidence interval bounds.
#' @param upper_ci Numeric vector of upper confidence interval bounds.
#' @param ci_level Confidence level to display in the label. Default is `0.95`
#'   (printed as `"95%"`).
#' @param estimate_suffix Suffix applied to the formatted point estimate
#'   (default is `"%"`). Use `""` for tables when `%` is already in the header.
#' @param bounds_suffix Suffix applied to the confidence interval bounds
#'   (default is `""`). Generally left blank to avoid repeating `%`.
#' @param dash Character string indicating how to display the separator between
#'   bounds. Options are `"en"` (en dash, default), `"rmd"` (two hyphens,
#'   which knit to an en dash in R Markdown), `"hyphen"`, or `"to"`.
#' @param pad_dash Logical; if `TRUE`, adds spaces around the dash (e.g.,
#'   `"41 – 49"` instead of `"41–49"`). Ignored when `dash = "to"`.
#' @param accuracy Rounding accuracy passed to [lamisc::fmt_pct()].
#' @param scale Multiplier for values before formatting. Default is `100`
#'   (for proportions stored as decimals).
#' @param prefix Character string prefix for formatted numbers (passed to
#'   [lamisc::fmt_pct()]).
#' @param big.mark Character used as thousands separator (passed to
#'   [lamisc::fmt_pct()]).
#' @param decimal.mark Character used as decimal point (passed to
#'   [lamisc::fmt_pct()]).
#' @param trim Logical; if `TRUE` (default), removes trailing zeros after
#'   decimal marks. Passed to [lamisc::fmt_pct()].
#' @param ... Additional arguments passed on to [lamisc::fmt_pct()].
#'
#' @return A character vector with formatted estimates and confidence intervals.
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#'
#' # Text style (default)
#' tibble(p = 0.45, lower_ci = 0.41, upper_ci = 0.49) |>
#'   mutate(p_ci = fmt_pct_ci(p, lower_ci, upper_ci, dash = "en"))
#' #> "45% (95% CI: 41–49)"
#'
#' # Table style (percent in header, Rmd-friendly dash)
#' tibble(p = 0.45, lower_ci = 0.41, upper_ci = 0.49) |>
#'   mutate(p_ci = fmt_pct_ci(p, lower_ci, upper_ci,
#'                            estimate_suffix = "", bounds_suffix = "", dash = "rmd"))
#' #> "45 (95% CI: 41--49)"
#'
#' # With padded en dash
#' tibble(p = 0.45, lower_ci = 0.41, upper_ci = 0.49) |>
#'   mutate(p_ci = fmt_pct_ci(p, lower_ci, upper_ci, dash = "en", pad_dash = TRUE))
#' #> "45% (95% CI: 41 – 49)"
#'
#' @export
fmt_pct_ci <- function(p, lower_ci, upper_ci,
                       ci_level = 0.95,
                       estimate_suffix = "%",   # "" for tables with % in header
                       bounds_suffix   = "",    # usually "" to avoid % in CI
                       dash = c("en", "rmd", "hyphen", "to"),
                       pad_dash = FALSE,
                       accuracy = NULL,
                       scale = 100,
                       prefix = "",
                       big.mark = "",
                       decimal.mark = ".",
                       trim = TRUE, ...) {

  dash <- match.arg(dash)
  dash_chr <- switch(dash,
                     en     = "\u2013",  # en dash
                     rmd    = "--",      # two hyphens -> en dash on knit
                     hyphen = "-",
                     to     = " to ")

  if (pad_dash && dash %in% c("en", "rmd", "hyphen")) {
    dash_chr <- paste0(" ", dash_chr, " ")
  }

  # CI label
  level_label <- paste0(round(ci_level * 100), "%")

  # format parts using your lamisc::fmt_pct
  est   <- lamisc::fmt_pct(p,
                           accuracy = accuracy,
                           scale = scale,
                           prefix = prefix,
                           suffix = estimate_suffix,
                           big.mark = big.mark,
                           decimal.mark = decimal.mark,
                           trim = trim, ...)
  lower <- lamisc::fmt_pct(lower_ci,
                           accuracy = accuracy,
                           scale = scale,
                           prefix = prefix,
                           suffix = bounds_suffix,
                           big.mark = big.mark,
                           decimal.mark = decimal.mark,
                           trim = trim, ...)
  upper <- lamisc::fmt_pct(upper_ci,
                           accuracy = accuracy,
                           scale = scale,
                           prefix = prefix,
                           suffix = bounds_suffix,
                           big.mark = big.mark,
                           decimal.mark = decimal.mark,
                           trim = trim, ...)


  out <- glue::glue("{est} ({level_label} CI: {lower}{dash_chr}{upper})")

  return(out)
}
