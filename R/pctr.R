#' @title
#' Format proportion (0 < p < 1) to a percent (0 < pct < 100)
#'
#' @description
#' Given a proportion (0 < p < 1), the function converts it to a percent (0 <
#' pct < 100) as either a numeric (when `as_text = FALSE`) or as a string with a
#' percent sign (`as_text = TRUE`). I owe this one to my buddy Kyle Hart.
#' `scales::percent()` has always been my preferred way to format percents but
#' this function has a little more flexibility and the number of decimal places
#' can be specified.
#'
#' @param x A numeric vector.
#' @param d integer indicating the number of decimal places (round).
#' @param as_text logical; if FALSE, a numeric value is returned; if TRUE, then
#'   a character is returned with a percent symbol
#' @param latex logical; default is `FALSE`. If `TRUE`, then the percent symbol is
#'   escaped.
#'
#' @return An object of similar structure to `x`
#'
#' @examples
#' pctr(x = runif(10), d = 1)
#' pctr(x = runif(10), d = 1, latex = TRUE)
#' pctr(x = runif(10), d = 1, as_text = FALSE)
#'
#' @name pctr-deprecated
#' @usage pctr(x, d, as_text, latex)
#' @seealso \code{\link{lamisc-deprecated}}
#' @keywords internal
NULL

#' @rdname lamisc-deprecated
#' @section \code{pctr}:
#' For \code{pctr}, use \code{\link{fmt_pct}}.
#'
#' @export
pctr <- function(x, d = 0, as_text = TRUE, latex = FALSE) {

  .Deprecated(msg = "'pctr' is deprecated. Please use 'fmt_pct' instead. Mind the changes in function arguments.")

  #### Percent character  --------------------------------
  if (latex) {
    pct_char <- "\\%"
  } else {
    pct_char <- "%"
  }

  #### Format percent --------------------------------
  if (as_text) {
    return(paste0(format(round(x * 100, d), nsmall = d), pct_char))
  } else {
    return(as.numeric(format(round(x * 100, d), nsmall = d)))
  }

}

