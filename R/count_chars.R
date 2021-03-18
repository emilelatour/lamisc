
#' @title
#' Character counting helper
#'
#' @description
#' Function to help count / index the characters on the screen, rather than
#' using your fingers.
#'
#' @param char a vector (atomic or list) or an expression object. Other objects
#'   (including classed objects) will be coerced by base::as.list.
#'
#' @references
#' https://rcrastinate.rbind.io/post/2020-12-26-little-helpers-character-index-counter/
#'
#' @return
#' A list
#'
#' @export
#'
#' @examples
#' count_chars(c("2020-12-26", "Klärschlammentsorgungsrichtlinie"))
count_chars <- function(char) {

  lapply(char, FUN = function (x) {
    spl <- strsplit(x, "")[[1]]
    ret <- 1:length(spl)
    names(ret) <- spl
    ret
  })

}
