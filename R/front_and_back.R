

#' Move selected columns to front and back
#'
#' Cool functions taken from @roman_francois Twitter post.
#' \url{https://gist.github.com/romainfrancois/5363c96646bb164f77fac2610d23b126}
#'
#'   Given a data frame or tibble, these functions will move selected columns to
#'   the front or to the back of the rest of the columns. Really a wrapper for
#'   \code{dplyr::select()} and \code{dplyr::everything()} but making cool use of
#'   tidyeval.
#'
#' @param data A data frame or tibble
#' @param ... Column(s) in the data frame to be moved
#'
#' @importFrom dplyr select everything
#' @importFrom purrr map
#' @importFrom rlang quos expr
#'
#' @return A data frame
#' @export
#'
#' @examples
#' library(dplyr)
#' back(iris, Species) %>% head
#' back(iris, Species, starts_with("Petal")) %>% head
#' front(iris, Species) %>% head

back <- function(data, ...) {
  dots <- rlang::quos(...)

  # negate each expression
  ndots <- purrr::map(.x = dots,
                      .f = function(q) expr(-!! q))

  # select the negated (rm the columns) and then select them back
  dplyr::select(data, !!! ndots, !!! dots)
}

#' @rdname back
#' @export
front <- function(data, ...) {
  dplyr::select(data, ..., everything())
}

