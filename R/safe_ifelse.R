

#' Preserves class when using ifelse
#'
#' This is just a cool, fun little function that I found on Stack Overflow,
#' \url{https://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects}.
#' Note that the function \code{dplyr::if_else()} is probably a better way to do
#' the same thing since it is unit-tested and dcoumented in a CRAN package. Just
#' preserving this here.
#'
#' @param cond A logical condition
#' @param true,false Values to use for `TRUE` and `FALSE` values of `cond`.
#'   Attributes are taken from `true`.
#'
#' @return Where `cond` is `TRUE`, the matching value from
#'   `true`, where it's `FALSE`, the matching value from `false`
#' @export
#'
#' @examples
#' dates <- as.Date(c("2011-01-01",
#'                    "2011-01-02",
#'                    "2011-01-03",
#'                    "2011-01-04",
#'                    "2011-01-05"))
#'
#' # This shows the issue with regular ifelse
#' dates <- ifelse(dates == "2011-01-01", dates - 1, dates)
#' dates
#' class(dates)
#'
#' # Try out safe_ifelse
#' dates <- safe_ifelse(dates == "2011-01-01", dates - 1, dates)
#' dates
#' class(dates)

safe_ifelse <- function(cond, true, false) {
  structure(ifelse(cond, true, false), class = class(true))
}
