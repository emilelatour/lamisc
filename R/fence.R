
#' @title
#' Limit values to a minimum and maximum
#'
#' @description
#' Clip / clamp / or fence, i.e. limit, values to a lower and upper bound.
#' Similar to `raster::clamp()`. This is a wrapper around `base::pmin()` and
#' `base::pmax()`; see those for more details.
#'
#' @references
#' https://stackoverflow.com/questions/13868963/clip-values-between-a-minimum-and-maximum-allowed-value-in-r
#'
#' @param x Vector of numeric values.
#' @param lower Numeric. Lowest value.
#' @param upper Numeric. Highest value.
#' @param na_rm A logical indicating whether missing values should be removed.
#'
#' @return
#' A numeric vector
#'
#' @export
#'
#' @examples
#' #### Example 1 --------------------------------
#'
#' vec <- sample(x = c(1:10),
#'        size = 10,
#'        replace = TRUE)
#'
#' vec
#'
#' fence(x = vec,
#'       lower = 4,
#'       upper = 6)
#'
#' #### Example 2 --------------------------------
#'
#' library(dplyr)
#' library(tibble)
#'
#' df <- tibble::tibble(
#'   vec = vec
#' )
#'
#' df %>%
#'   mutate(vec = fence(x = vec,
#'                      lower = 4,
#'                      upper = 6))

fence <- function(x, lower = -Inf, upper = Inf, na_rm = FALSE) {

  pmax(lower, na.rm = na_rm,
       pmin(x, upper, na.rm = na_rm))

}
