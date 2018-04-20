

#' Not in
#'
#' Creating %not_in% with purrr::negate. Adapted from the cool, on going, Your
#' daily dose of {purrr} from Colin Fay.
#' \url{https://twitter.com/_ColinFay/status/987260548344631298/photo/1}
#'
#'
#' importFrom base `%in`
#'
#' @return
#' @export
#'
#' @examples
#' 1 %not_in% 1:10
#' 11 %not_in% 1:10

`%not_in%` <- purrr::negate(`%in%`)


