
#' @title
#' A rounding function with option to return character
#'
#' @description
#' A simple rounding function that returns exactly the number of digits
#' requested. Really just a wrapper for format and round.
#'
#' Credit for this one goes to my buddy Kyle Hart. I had seen a few different
#' versions of a function like this and I really like his for the flexibility
#' that it offers by formatting as a character. In the end, I took his basic
#' function and took it a few steps further to be the way that I want it to be,
#' like the option to trim whitespace.
#'
#' @param x a numeric vector.
#' @param d integer indicating the number of decimal places (round). Negative
#'   values are allowed (see 'Examples').
#' @param as_text logical; if FALSE, a numeric value is returned; if TRUE, then
#'   a character is returned with values aligned at the decimal point and padded
#'   with spaces in front (use `trim` option to keep or remove spaces)
#' @param trim logical; if FALSE, logical, numeric and complex values are
#'   right-justified to a common width; if TRUE the leading blanks for
#'   justification are suppressed. Only has an effect if `as_text` is TRUE.
#'
#' @details
#' Rounding to a negative number of digits means rounding to a power of ten.
#'
#' @return An object of similar structure to `x`
#'
#' @examples
#' numbers <- c(0.0018, 0.1234, 12.6978, -86.75309, 264.4445, -99.9999)
#' roundr(numbers, d = 2, as_text = TRUE)
#' roundr(numbers, d = 2, as_text = TRUE, trim = TRUE)
#' roundr(numbers, d = 2, as_text = FALSE)
#' roundr(numbers, d = -2, as_text = FALSE)
#' roundr(numbers, d = -2, as_text = TRUE, trim = TRUE)
#' roundr(numbers, d = -2, as_text = TRUE, trim = FALSE)
#'
#' numbers <- c(10.00001, 12345, 1234.5, NA, 1.2345, 0.12345)
#' roundr(numbers, d = 2, as_text = TRUE)
#' roundr(numbers, d = 2, as_text = TRUE, trim = TRUE)
#' roundr(numbers, d = 2, as_text = FALSE)
#' roundr(numbers, d = -2, as_text = FALSE)
#' roundr(numbers, d = -2, as_text = TRUE, trim = TRUE)
#' roundr(numbers, d = -2, as_text = TRUE, trim = FALSE)
#'
#' @name roundr-deprecated
#' @usage roundr(x, d, as_text, trim)
#' @seealso \code{\link{lamisc-deprecated}}
#' @keywords internal
NULL

#' @rdname lamisc-deprecated
#' @section \code{roundr}:
#' For \code{roundr}, use \code{\link{fmt_num}}.
#'
#' @export
roundr <- function(x, d = 3, as_text = FALSE, trim = FALSE) {

  .Deprecated(msg = "'roundr' is deprecated. Please use 'fmt_num' instead. Mind the changes in function arguments.")

  if (as_text == FALSE) {
    as.numeric(format(round(x, d), nsmall = max(0, d), scientific = FALSE))
  } else {
    format(round(x, d), nsmall = max(0, d), scientific = FALSE, trim = trim)
  }

}
