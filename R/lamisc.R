#' @title
#' lamisc: Miscellaneous helper functions
#'
#' @description
#' The objective of the package is to be a landing spot for the miscellaneous
#' functions that I end up writing and want to preserve in one location.
#'
#' @examples
#' # Example usage:
#' library(lamisc)
#'
#' @docType package
#' @name lamisc


## quiets concerns of R CMD check re: the .'s that appear in pipelines
## From Jenny Bryan's googlesheets package
## From infer package
## https://github.com/tidymodels/infer/blob/master/R/infer.R
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ".", "surv", "res", "events", "group", "median", "n_start", "name",
      "value", "x0_95lcl", "x0_95ucl", "strata", "time_at_risk", "key",
      "lower_ci", "upper_ci", "rate", "se"
    )
  )
}


#### %||% --------------------------------

# From Jim Hester

"%||%" <- function(x, y) if (is.null(x)) y else x # nocov

#### force_all --------------------------------

# from scales package

# Evaluates all arguments (see #81)
force_all <- function(...) list(...)



# ggladder: no visible binding for global variable ‘inv_square_root’
#   ggladder: no visible binding for global variable ‘inverse’
#   ggladder: no visible binding for global variable ‘inv_square’
#   ggladder: no visible binding for global variable ‘inv_cubic’
#   ladder: no visible binding for global variable ‘adj_chi2’
#   ladder: no visible binding for global variable ‘p_value’
#   make_histos: no visible binding for global variable ‘sqrt_n’
#   make_histos: no visible binding for global variable ‘ten_log_10’
#   make_histos: no visible binding for global variable ‘..density..’
#   make_histos: no visible binding for global variable ‘dnorm’
#   qqladder: no visible binding for global variable ‘inv_square_root’
#   qqladder: no visible binding for global variable ‘inverse’
#   qqladder: no visible binding for global variable ‘inv_square’
#   qqladder: no visible binding for global variable ‘inv_cubic’
#   Undefined global functions or variables:
#     ..density.. adj_chi2 dnorm inv_cubic inv_square inv_square_root
#     inverse p_value sqrt_n ten_log_10


NULL
