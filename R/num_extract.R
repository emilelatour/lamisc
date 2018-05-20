
#' @title
#' Extraction of numbers from a character string
#'
#' @description
#' Simple wrapper for some regex to extract only the numbers from a character
#' strin. Source: \url{http://stla.github.io/stlapblog/posts/Numextract.html}.
#' There are some other pretty cool functions after the link too.
#'
#' @param string A value or vector with mix of numbers and characters
#' @param as_char Logical; if \code{TRUE}, returns a character, and if
#'   \code{FALSE} a numeric.
#'
#' @importFrom stringr str_extract
#' @importFrom stringr str_trim
#'
#' @return A vector or value; character or numeric
#' @export
#'
#' @examples
#' num_extract("30.5ml")
#'
#' pvals <- c('0.00000', '< 0.001', '0.0', '0.123', '0.6', '1', '1.0', '1.000')
#' num_extract(pvals)
#' num_extract(pvals, as_char = FALSE)
#'
#' negatives <- c(-0.133213, -0.06023, -0.004233, -0.000000134234, -1)
#' num_extract(negatives)

num_extract <- function(string, as_char = TRUE){

  if (as_char)
    stringr::str_trim(
      format(stringr::str_extract(string, "\\-*\\d+\\.*\\d*"),
             scientific = FALSE,
             trim = TRUE)
    )

  else
    as.numeric(
      stringr::str_trim(
        format(stringr::str_extract(string, "\\-*\\d+\\.*\\d*"),
               scientific = FALSE,
               trim = TRUE)
        )
      )

}
