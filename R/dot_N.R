#' @title
#' Use `Rmpfr` to use mpfr-number instead of a simple number to be more accurate
#'
#' @description
#' This wrapper comes pretty much straight from the vignette. When you need
#' higher precision estimates than R will normally allow, this will help out.
#'
#' By using Rmpfr, you can often call your R function or numerical code with
#' mpfr–numbers instead of simple numbers, and all results will automatically be
#' much more accurate.
#'
#' @references
#' \url{https://cran.r-project.org/web/packages/Rmpfr/vignettes/Maechler_useR_2011-abstr.pdf}
#'
#' @param x A number or vector of numbers
#' @param precBits a number, the maximal precision to be used, in bits; i.e. 53
#'   corresponds to double precision. Must be at least 2. If missing, getPrec(x)
#'   determines a default precision.
#'
#' @importFrom Rmpfr mpfr
#'
#' @return An mpfr-number or vector of mpfr-numbers
#' @export
#'
#' @examples
#' options(digits = 17)  # to print to full "standard R" precision
#' exp(1)
#' exp(.N(1))
#'
.N <- function(x, precBits = 200) {
  Rmpfr::mpfr(x, precBits = precBits)
}
