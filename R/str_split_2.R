


#' strsplit 2.0 by Jakob Gepp
#'
#' Splits the string but keeps the delimiter. Adopted from
#' \url{https://www.statworx.com/de/blog/strsplit-but-keeping-the-delimiter/}.
#'
#' A regular expression is a pattern that describes a set of strings. Two types
#' of regular expressions are used in R, extended regular expressions (the
#' default) and Perl-like regular expressions used by perl = TRUE. There is a
#' also fixed = TRUE which can be considered to use a literal regular
#' expression.
#'
#' @param x A character vector or string
#' @param split What to split the string by
#' @param type Select one of \code{type = c("remove", "before", "after")}
#' @param perl Extended results expresions are the default \code{perl = FALSE}
#'    and \emph{Perl-like} regular expression with \code{perl = TRUE}
#' @param ... Other arguments
#'
#' @return A list of characters
#' @export
#'
#' @examples
#' x <- c("3D/MON&SUN")
#' str_split_2(x, "(?<=.)(?=[/&])",perl = TRUE)
#'
#' a <- c("asdasd.sss","segssddfge.sss","se.sss")
#' str_split_2(x = a, split = "[.]", type = "remove", perl = TRUE)
#'
str_split_2 <- function(x,
                        split,
                        type = "remove",
                        perl = FALSE,
                        ...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}

