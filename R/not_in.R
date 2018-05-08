

#' @title
#' Not in
#'
#' @description
#' Creating %not_in% with purrr::negate. Adapted from the cool, on going, Your
#' daily dose of {purrr} from Colin Fay.
#'
#' @references
#' \url{https://twitter.com/_ColinFay/status/987260548344631298/photo/1}
#'
#' @name %not_in%
#' @rdname grapes-not_in-grapes
#' @return A logical
#' @export
#'
#' @param x vector or NULL: the values to be matched. Long vectors are supported.
#' @param table vector or NULL: the values to be matched against. Long vectors are not supported.
#'
#' @usage x \%not_in\% table
#'
#' @examples
#' 1 %not_in% 1:10
#' 11 %not_in% 1:10

`%not_in%` <- purrr::negate(base::`%in%`)


