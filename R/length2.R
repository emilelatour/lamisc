

#' @title
#' Length that can handle NAs
#'
#' @description
#' Version of length which can handle `NA`s: if `na.rm == TRUE`, don't count
#' them
#'
#' @param x A vector
#' @param na_rm Boolean; Default is `FALSE` which includes `NA`s. If `TRUE` `NA`s
#'   will not be counted.
#'
#' @return
#' an integer value
#'
#' @export
#'
#' @examples
#' length2(x = c(1, 2, 3, NA, 5, 6, NA, 7, 8, NA, 10))
#' length2(x = c(1, 2, 3, NA, 5, 6, NA, 7, 8, NA, 10), na_rm = TRUE)

length2 <- function(x, na_rm = FALSE) {
  if (na_rm) {
    sum(!is.na(x))
  } else {
    length(x)
  }
}
