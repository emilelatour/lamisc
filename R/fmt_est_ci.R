#' Format numeric estimates with confidence intervals
#'
#' @description
#' A convenience wrapper around [lamisc::fmt_num()] (or similar numeric
#' formatter) and [glue::glue()] to format point estimates and confidence
#' intervals consistently for text and tables.
#' By default, the output looks like `"12.3 (95% CI: 9.8–14.7)"`.
#'
#' @param est Numeric vector of point estimates.
#' @param lower_ci Numeric vector of lower confidence interval bounds.
#' @param upper_ci Numeric vector of upper confidence interval bounds.
#' @param ci_level Confidence level to display in the label. Default is `0.95`
#'   (printed as `"95%"`).
#' @param estimate_suffix Suffix applied to the formatted point estimate.
#'   Default is `""`.
#' @param bounds_suffix Suffix applied to the confidence interval bounds.
#'   Default is `""`.
#' @param show_ci_label Logical; if TRUE (default), includes the confidence level
#'   label (e.g., "95% CI:") before the interval bounds. If FALSE, only the
#'   bounds are shown, producing output like `"12.3 (9.8–14.7)"`.
#' @param dash Character string indicating how to display the separator between
#'   bounds. Options are `"en"` (en dash, default), `"rmd"` (two hyphens, which
#'   knit to an en dash in R Markdown), `"hyphen"`, or `"to"`.
#' @param pad_dash Logical; if `TRUE`, adds spaces around the dash (e.g.,
#'   `"9.8 – 14.7"` instead of `"9.8–14.7"`). Ignored when `dash = "to"`.
#' @param accuracy Rounding accuracy passed to [lamisc::fmt_num()].
#' @param scale Multiplier for values before formatting. Default is `1`.
#' @param prefix Character string prefix for formatted numbers (passed to
#'   [lamisc::fmt_num()]).
#' @param big.mark Character used as thousands separator (passed to
#'   [lamisc::fmt_num()]).
#' @param decimal.mark Character used as decimal point (passed to
#'   [lamisc::fmt_num()]).
#' @param ci_brackets Character string giving the characters used to enclose the
#'   confidence interval. Default is `"()"`; use `"[]"` for square brackets or
#'   any two-character string (e.g., `"<>"`).
#' @param trim Logical; if `TRUE` (default), removes trailing zeros after decimal
#'   marks. Passed to [lamisc::fmt_num()].
#' @param ... Additional arguments passed on to [lamisc::fmt_num()].
#'
#' @return A character vector with formatted estimates and confidence intervals.
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#'
#' # Default: text-style output
#' tibble(est = 12.3, lower_ci = 9.8, upper_ci = 14.7) |>
#'   mutate(est_ci = fmt_est_ci(est, lower_ci, upper_ci, accuracy = 0.1))
#' #> "12.3 (95% CI: 9.8–14.7)"
#'
#' # Without CI label, with spaces around en dash
#' tibble(est = 12.3, lower_ci = 9.8, upper_ci = 14.7) |>
#'   mutate(est_ci = fmt_est_ci(est, lower_ci, upper_ci,
#'                              accuracy = 0.1,
#'                              show_ci_label = FALSE,
#'                              pad_dash = TRUE))
#' #> "12.3 (9.8 – 14.7)"
#'
#' @export
fmt_est_ci <- function(est, lower_ci, upper_ci,
                       ci_level = 0.95,
                       estimate_suffix = "",
                       bounds_suffix   = "",
                       show_ci_label = TRUE,
                       dash = c("en", "rmd", "hyphen", "to"),
                       pad_dash = FALSE,
                       accuracy = NULL,
                       scale = 1,
                       prefix = "",
                       big.mark = "",
                       decimal.mark = ".",
                       ci_brackets = "()",
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

  # Brackets
  left_bracket  <- substr(ci_brackets, 1, 1)
  right_bracket <- substr(ci_brackets, 2, 2)

  # CI label
  level_label <- paste0(round(ci_level * 100), "%")

  # Format numeric parts
  est_f   <- lamisc::fmt_num(est,
                             accuracy = accuracy,
                             scale = scale,
                             prefix = prefix,
                             suffix = estimate_suffix,
                             big.mark = big.mark,
                             decimal.mark = decimal.mark,
                             trim = trim, ...)
  lower_f <- lamisc::fmt_num(lower_ci,
                             accuracy = accuracy,
                             scale = scale,
                             prefix = prefix,
                             suffix = bounds_suffix,
                             big.mark = big.mark,
                             decimal.mark = decimal.mark,
                             trim = trim, ...)
  upper_f <- lamisc::fmt_num(upper_ci,
                             accuracy = accuracy,
                             scale = scale,
                             prefix = prefix,
                             suffix = bounds_suffix,
                             big.mark = big.mark,
                             decimal.mark = decimal.mark,
                             trim = trim, ...)

  if (show_ci_label) {
    out <- glue::glue("{est_f} {left_bracket}{level_label} CI: {lower_f}{dash_chr}{upper_f}{right_bracket}")
  } else {
    out <- glue::glue("{est_f} {left_bracket}{lower_f}{dash_chr}{upper_f}{right_bracket}")
  }

  return(out)
}
