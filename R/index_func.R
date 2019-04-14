
#' @title
#' Count the occurence of a value in a vector.
#'
#' @description
#' For when you want to know which occurence a value is in a vector and not just
#' the number of times that it was duplicated.
#'
#' @importFrom purrr map_int
#'
#' @param a A vector or column in a data frame
#'
#' @return
#' A vector of counts that increase with each appearance of a value in a vector.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' foo <- tibble::tribble(
#'   ~numbers, ~numbers2,
#'         4L,        4L,
#'        23L,       23L,
#'         4L,        4L,
#'        23L,       23L,
#'         5L,        5L,
#'        43L,       43L,
#'        54L,       54L,
#'        56L,       56L,
#'       657L,      657L,
#'        67L,       67L,
#'        67L,       67L,
#'       435L,      435L,
#'       453L,      453L,
#'       435L,      435L,
#'       324L,      324L,
#'        34L,       34L,
#'       456L,      456L,
#'        56L,       56L,
#'       567L,      567L,
#'        65L,       65L,
#'        34L,       34L,
#'       435L,      435L
#'   )
#'
#' foo %>%
#'   mutate(index = index_func(numbers))

index_func <- function(a) {
  purrr::map_int(.x = 1:length(a),
                 .f = ~ sum(a[1:.x] == a[.x]))
}


# Other ways to do it
# foo %>%
#   mutate(index = sapply(1:length(numbers), function(x)
#     sum(numbers[1:x] == numbers[x])))
#
# foo %>%
#   mutate(index = purrr::map_int(
#     .x = 1:length(numbers),
#     .f = ~ sum(numbers[1:.x] == numbers[.x])
#   ))




