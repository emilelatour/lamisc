






#' @title
#' A function for formatting p-values
#'
#' @description
#'
#' Similar to the base R function `format.pval()` but does things the way that I
#' want them. Inspiration and lots of help with this one owed to
#' \url{https://github.com/raredd/rawr}.
#'
#' @param pvals A numeric value or vector of p-values
#' @param sig_limit Lower bound for precisions
#' @param d Integer indicating the number of decimal places (round)
#' @param html Logical; if \code{TRUE}, uses \code{&lt;} instead of \code{<}.
#'
#' @return A character value or vector of formatted p-values
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' pvals <- c(.133213, .06023, .004233, .000000134234, 1.0)
#' pvals_2 <- c(.133213, .06023, .004233, .000000134234, NA)
#' pvalr(pvals)
#' pvalr(pvals_2)
#'
#' df <- tibble::tibble(p = pvals,
#'                      p_2 = pvals_2)
#'
#' df %>%
#'   mutate(p_formatted = pvalr(p),
#'          p_2_formatted = pvalr(p_2)
#'   )
#'
pvalr <- function(pvals, sig_limit = 0.001, d = 3L, html = FALSE) {

  stopifnot(
    sig_limit > 0,
    sig_limit < 1
  )

  html <- html + 1L

  sapply(pvals, function(x, sig_limit) {

    if (is.na(x) | !nzchar(x)) return(NA)

    if (x < sig_limit) {
      sprintf(c('< %s', '&lt; %s')[html], format(sig_limit))
    } else {
      return(lamisc::roundr(x, d = d, as_text = TRUE, trim = FALSE))
    }

  }, sig_limit = sig_limit)

}



