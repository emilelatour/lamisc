

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
#' A couple of reference:
#'   + \url{https://stackoverflow.com/questions/45070117/r-check-if-special-character-in-string}
#'   + \url{https://stackoverflow.com/questions/36928870/r-check-if-string-contains-special-characters}
#'
#' @param string The string
#' @param char A character used to delimit the string
#'
#' @return A string
#' @export
#'
#' @examples
#' a <- c("asdasd.sss","segssddfge.sss","se.sss")
#' left(a, ".")
#' right(a, ".")
#'
#'

left <- function(string, char) {

  special <- pattern <- "/|:|\\?|<|>|\\|\\\\|\\*\\."

  if (grepl(char, special)) {
    esc <- "\\"
  } else {
    esc <- ""
  }

  substr(string,
         start = 1,
         stop = unlist(gregexpr(paste0(esc, char), string)) - 1
  )

}


#' @rdname left
#' @export
right <- function(string, char) {

  special <- pattern <- "/|:|\\?|<|>|\\|\\\\|\\*\\."

  if (grepl(char, special)) {
    esc <- "\\"
  } else {
    esc <- ""
  }

  substr(string,
         start = unlist(gregexpr(paste0(esc, char), string)) + 1,
         stop = nchar(string)
  )

}

