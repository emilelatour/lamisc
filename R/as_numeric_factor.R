
#' @title
#' Convert a factor to integer/numeric without loss of information
#'
#' @description
#' To transform a factor `f` to approximately its original numeric values,
#' `as.numeric(levels(f))[f]` is recommended and slightly more efficient than
#' `as.numeric(as.character(f))`.
#'
#' @references
#' https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
#' https://cran.r-project.org/doc/FAQ/R-FAQ.html#How-do-I-convert-factors-to-numeric_003f
#'
#' @param x an object, for example a factor.
#'
#' @return A numeric object
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' df <- tibble::tibble(
#'   foo = factor(sample(x = c(4, 5, 6),  size = 50, replace = TRUE),
#'                levels = c(6, 5, 4))
#' )
#'
#' df
#'
#' df %>%
#'   mutate(foo_base = as.numeric(foo),
#'          foo_lamisc = as_numeric_factor(foo),
#'          foo_alt = as.numeric(as.character(foo)))

as_numeric_factor <- function(x) {
  as.numeric(levels(x))[x]
}



