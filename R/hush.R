#' @title
#' Sometimes you just need a little peace and quiet.
#'
#' @description
#' This uses `sink()` to silence unwanted output from R code. So far it seems to
#' be more successful than `invisible()`. I basically followed the Stack
#' Overflow discussion in the references to create this one.
#'
#' @references
#'
#' \url{https://stackoverflow.com/questions/2723034/suppress-one-commands-output-in-r}
#'
#' @param code R code that you want to silence.
#' @param is_unix Logical; Default is FALSE, but if you are working in a UNIX
#'   environment then you will want to set it to TRUE.
#'
#' @return The output of your code, but without printing to console.
#' @export
#'
#' @examples
#' x <- dput(names(mtcars))
#' x <- hush(dput(names(mtcars)))
#' x

hush <- function(code, is_unix = FALSE) {

  if (is_unix == TRUE) {
    sink("/dev/null") # use /dev/null in UNIX
  } else {
    sink("NUL") # use /dev/null in UNIX
  }

  tmp <- code

  sink()

  return(tmp)
}


