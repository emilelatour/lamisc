
#' @title
#' Counting, combinations, and permutations
#'
#' @description
#' Functions to determine the number of possible ways to choose a sample
#' with/without replacement where order does and does not matter.
#'
#' Without replacement, if the order doesn't matter then we have a combination,
#' if the order do matter then we have a permutation. One could say that a
#' permutation is an ordered combination.
#'
#' @param n total number of objects in the set; an integer.
#' @param k number of choosing objects from the set; an integer
#'
#' @references
#' https://www.mathsisfun.com/combinatorics/combinations-permutations.html
#'
#' @return
#' A numeric value.
#'
#' @export
#'
#' @examples
#' perm(15, 2)
#' comb(15, 2)
#'
#' # same as comb(15, 2)
#' my_sample(n = 15,
#'           k = 2,
#'           replace = FALSE,
#'           ordered = FALSE)
#'
#' # same as perm(15, 2)
#' my_sample(n = 15,
#'           k = 2,
#'           replace = FALSE,
#'           ordered = TRUE)
#'
#' my_sample(n = 15,
#'           k = 2,
#'           replace = TRUE,
#'           ordered = TRUE)
#'
#' my_sample(n = 15,
#'           k = 2,
#'           replace = TRUE,
#'           ordered = FALSE)

#' @rdname my_sample
#' @export
#' @param replace Logical indicating with or without replacement; default is
#'   `TRUE`.
#' @param ordered Logical indicating ordered or unordered; default is
#'   `TRUE`.
my_sample <- function(n, k, replace = TRUE, ordered = TRUE) {

  if (replace == TRUE & ordered == TRUE) {
    # With replacement, ordered
    n ^ k

  } else if (replace == FALSE & ordered == TRUE) {
    # Without replacement, ordered
    # Permutation
    factorial(n) / factorial(n - k)

  } else if (replace == TRUE & ordered == FALSE) {
    # With replacement, ordered
    factorial(n + k - 1) / factorial(n - 1) / factorial(k)

  } else if (replace == FALSE & ordered == FALSE) {
    # Without replacement, unordered
    # Combination
    factorial(n) / factorial(n - k) / factorial(k)

  }

}

#' @rdname my_sample
#' @export
perm <- function(n, k) {
  factorial(n) / factorial(n - k)
}

#' @rdname my_sample
#' @export
comb <- function(n, k) {
  factorial(n) / factorial(n - k) / factorial(k)
}


