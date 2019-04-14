
#' @title
#' Lookup and retrieve data from a specific column in table
#'
#' @description
#' If you've ever used excel, then you are hopefully familiar with the useful
#' `vlookup`. This is a fun little version of it in R that was tweeted by Jenny
#' Bryan at some point.
#'
#' Match on the `key` and return `value`.
#'
#' @references
#' \url{https://twitter.com/JennyBryan/status/980978609794895872/photo/1}
#'
#' @param this Value or vector; what you are trying to look up
#' @param data Data frame; the data where you want to match `this`
#' @param key The name (quoted) of the column to match on
#' @param value The name (quoted) of the column to return
#'
#' @return A vector of values
#' @export
#'
#' @examples
#' library(dplyr)
#' head(starwars)
#'
#' c("Luke Skywalker", "Jabba Desilijic Tiure", "Yoda") %>%
#'   vlookup(this = .,
#'           data = starwars,
#'           key = "name",
#'           value = "species")
#'
#' c("Luke Skywalker", "Jabba Desilijic Tiure", "Yoda") %>%
#'   vlookup(this = ., data = starwars, key = "name", value = "mass") %>%
#'   sum(.)
#'
#' c("Luke Skywalker", "Jabba Desilijic Tiure", "Yoda") %>%
#'   vlookup(data = starwars, key = "name", value = "mass") %>%
#'   sum(.)

vlookup <- function(this, data, key, value) {
  m <- match(this, data[[key]])
  data[[value]][m]
}


