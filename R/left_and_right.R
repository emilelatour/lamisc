#' @title
#' Return text to the left and the right of a string
#'
#' @description
#' `left` takes a string and a character as arguments, then returns the
#' characters to the left. `right` takes a string and a character as
#' arguments, then returns the characters to the right.
#'
#' Wrappers around some regular expressions. `Left` and `right` operate
#' similar to combining `left()`/`right()` and `search()` in excel. So maybe
#' there's a bit of nostalgia for me in using these functions. But I also
#' think that the naming makes it clearer what is going on.
#'
#' @references
#' \url{https://stackoverflow.com/questions/45070117/r-check-if-special-character-in-string}
#'
#' \url{https://stackoverflow.com/questions/36928870/r-check-if-string-contains-special-characters}
#'
#' @param string The string
#' @param char A character used to delimit the string
#' @param pattern Special characters that need to be escaped. Default is
#'   "/|:|\\?|<|>|\\|\\\\|\\*\\.()"
#'
#' @importFrom stringr str_sub
#' @importFrom stringr str_locate
#'
#' @return A string
#' @export
#'
#' @examples
#' a <- c("left.right","left.right","left.right")
#' left(a, ".")
#' right(a, ".")
#'
#' library(dplyr)
#' library(tibble)
#'
#' test_df <- tibble::tibble(
#'   label = c("resection_margin=Mohs, > 1 cm",
#'             "resection_margin=Mohs, > 1 cm",
#'             "resection_margin=Mohs, <= 1 cm",
#'             "  resection_margin=Mohs, > 1 cm"))
#' test_df %>%
#'   mutate(left = left(label, "="),
#'          right = right(label, "="))
#'
#' right(test_df$label, "=")
#' left(test_df$label, "=")
#' stringr::str_trim(left(test_df$label, "="), side = "both")
#'

left <- function(string, char, pattern = "/|:|\\?|<|>|\\|\\\\|\\*\\.()") {

  special <- pattern

  if (grepl(char, special)) {
    esc <- "\\"
  } else {
    esc <- ""
  }

  # substr(string,
  #        start = 1,
  #        stop = unlist(gregexpr(paste0(esc, char), string)) - 1)

  stringr::str_sub(string,
                   start = 1,
                   end = stringr::str_locate(string,
                                               paste0(esc, char))[, 1] - 1)

}


#' @rdname left
#' @export
right <- function(string, char, pattern = "/|:|\\?|<|>|\\|\\\\|\\*\\.()") {

  special <- pattern

  if (grepl(char, special)) {
    esc <- "\\"
  } else {
    esc <- ""
  }

  # substr(string,
  #        start = unlist(gregexpr(paste0(esc, char), string)) + 1,
  #        stop = nchar(string))

  stringr::str_sub(string,
                   start = stringr::str_locate(string,
                                               paste0(esc, char))[, 1] + 1,
                   end = nchar(string))


}





