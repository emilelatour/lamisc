#' lamisc: Miscellaneous helper functions
#'
# The objective of the package is to be a landing spot for the miscellaneous
# functions that I end up writing and want to preserve in one location.
#'
#' @examples
#' # Example usage:
#' library(lamisc)
#'
#' @docType package
#' @name lamisc
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## From Jenny Bryan's googlesheets package
## From infer package
## https://github.com/tidymodels/infer/blob/master/R/infer.R
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ".", "surv", "res", "events", "group", "median", "n_start", "name",
      "value", "x0_95lcl", "x0_95ucl", "strata", "time_at_risk", "key"
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
