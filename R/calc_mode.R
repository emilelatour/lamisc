
#' @title
#' Mode -- most frequent value of a variable
#'
#' @description
#' Caclulate and return the most frequently occuring value of a vector.
#'
#' @param x A vector of values
#' @param na.rm a logical value indicating whether `NA` values should be
#'   stripped before the computation proceeds.
#'
#' @return
#' Returns the most frequent value. If there are more than one, all of them are
#' returned in a vector.
#'
#' @references
#' https://www.tutorialspoint.com/r/r_mean_median_mode.htm
#' https://rdrr.io/cran/DescTools/man/Mode.html
#'
#' @export
#'
#' @examples
#' # Create the vector with numbers.
#' x <- c(2, 1, 2, 3, 1, 2, 3, 4, 1, 5, 5, NA, 2, 3)
#'
#' # Calculate the mode using the user function.
#' calc_mode(x, na.rm = TRUE)
#' calc_mode(x, na.rm = FALSE)
#'
#' library(DescTools)
#' data(d.pizza)
#'
#' calc_mode(d.pizza$driver)
#' sapply(d.pizza[,c("driver","temperature","date")], calc_mode, na.rm = TRUE)
#'
#' # Two values are returned if more than one mode
#' y <- c(2, 2, 2, 3, 3, 3)
#' calc_mode(y)

calc_mode <- function(x, na.rm = FALSE) {

  if (!is.atomic(x) | is.matrix(x)) {
    stop("Mode supports only atomic vectors. Use sapply(*, Mode) instead.")
  }

  if(na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  # uniqv <- unique(x)
  # res <- uniqv[which.max(tabulate(match(x, uniqv)))]

  tab <- table(x)
  res <- names(which(tab == max(tab)))

  if (!inherits(x, "factor")) {
    class(res) <- class(x)
  }

  as.vector(res)

}

