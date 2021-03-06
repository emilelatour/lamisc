

#' Number of unique values
#'
#' Get the number of unique values in a vector. Borrowed from Karl Borman.
#' Source: \url{https://github.com/kbroman/broman/blob/master/R/lenuniq.R}
#'
#' @param vec A vector
#' @param na.rm If \code{TRUE}, remove any missing values
#'
#' @return Number of unique values.
#'
#' @details It just does \code{length(unique(vec))} or, if
#' \code{na.rm=TRUE} (the default)
#' \code{length(unique(vec[!is.na(vec)]))}
#'
#' @export
#' @keywords utilities
#' @examples
#' x <- c(1, 2, 1, 3, 1, 1, 2, 2, 3, NA, NA, 1)
#' len_uniq(x)
#' len_uniq(x, na.rm=FALSE)

len_uniq <- function(vec, na.rm = TRUE)
  {
    if (na.rm && !is.null(vec)) vec <- vec[!is.na(vec)]
    length(unique(vec))
  }
