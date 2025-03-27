#' Replace repeated values with a replacement string
#'
#' This function checks whether each element in a vector `x` is the same as the element directly above it
#' (i.e., the previous value in the vector). If so, it replaces that element with a specified replacement value.
#'
#' This is often useful for visual display in tables, where repeated values in consecutive rows can be suppressed
#' for readability.
#'
#' @param x A vector (typically character or factor) to check for repeated consecutive values.
#' @param replacement A value (default is `""`) to replace repeated values with.
#'
#' @return A character vector where values that are the same as the previous value are replaced with `replacement`.
#' The first element is always retained.
#'
#' @examples
#' check_above(c("A", "A", "B", "B", "B", "C"))
#' # Returns: "A" "" "B" "" "" "C"
#'
#' @export
check_above <- function(x,
                        replacement = "") {

  check <- (x == dplyr::lag(x))
  check[1] <- FALSE

  x <- as.character(x)
  x[check] <- replacement

  return(x)

}
