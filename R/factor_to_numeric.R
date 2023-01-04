
#' @title
#' Convert a factor to integer/numeric
#'
#' @description
#' Different than `lamisc::as_numeric_factor()`. This function returns
#' corresponding ordinal level value for the factor values. For a factor `f`, it
#' works like you would think that `as.numeric(f)` or
#' `as.numeric(as.character(f))` would work (both of these give an error). This
#' one is hard to explain but see the example.
#'
#' @param x an object, for example a factor.
#'
#' @return A numeric object
#'
#' @export
#'
#' @examples
#' x1 <- c("Dec", "Apr", "Jan", "Mar")
#'
#' month_levels <- c(
#'   "Jan", "Feb", "Mar", "Apr", "May", "Jun",
#'   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
#' )
#'
#' y1 <- factor(x1, levels = month_levels)
#' y1
#'
#' factor_to_numeric(y1)
#'
factor_to_numeric <- function(x) {
  match(x, levels(x))
}

