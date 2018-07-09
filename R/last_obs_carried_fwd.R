
#' @title
#' A "Last Observation Carried Forward" function
#'
#' @description
#' Repeat the last non NA value. "Fill forward" NAs with the closest previous
#' non-NA value. There are lots of alternatives out there to this function. My
#' fave is \code{tidyr::fill()}.
#'
#' @references
#' \url{https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value}
#'
#' @param x A vector
#' @param forward Logical; default is `TRUE`. Direction to carry forward. By
#'   specifying forward = FALSE, you can carry the last observation backward.
#' @param maxgap Numeric; By specifying maxgap, you can choose not to bridge
#'   overly long gaps.
#' @param na.rm Logical; default is `FALSE`. Whether to remove NAs.
#'
#' @return A similar vector to x
#' @export
#'
#' @examples
#' repeat_last(c(1,2,3,4,NA,NA))
#' repeat_last(c(1,NA,3,4,NA,NA), forward = FALSE)
#' repeat_last(c(1,NA,3,4,NA,NA, 5), forward = FALSE)
#'
#' x = c(NA, NA, 1, NA, NA, NA, NA, NA, NA, 2, 3, 4, NA, NA, NA, NA, NA, 5, NA)
#' data.frame(x,
#'            repeat_last(x),
#'            repeat_last(x, forward = FALSE),
#'            repeat_last(x, maxgap = 5),
#'            check.names = FALSE)

repeat_last = function(x, forward = TRUE, maxgap = Inf, na.rm = FALSE) {

  # Reverse x twice if carrying backward
  if (!forward) x = rev(x)

  # Get positions of nonmissing values
  ind = which(!is.na(x))

  # If it begins with NA add first pos
  if (is.na(x[1]) && !na.rm)
    ind = c(1, ind)

  # Diffing the indices + length yields how often they need to be repeated
  rep_times = diff(c(ind, length(x) + 1))

  # Do any exceed the maxgap? Add NA in gaps. Diff again
  if (maxgap < Inf) {
    exceed = rep_times - 1 > maxgap
    if (any(exceed)) {
      ind = sort(c(ind[exceed] + 1, ind))
      rep_times = diff(c(ind, length(x) + 1))
    }
  }

  # Repeat the values at these indices
  x = rep(x[ind], times = rep_times)

  # Reverse again if carrying backwars
  if (!forward) x = rev(x)

  # Return x
  x
}

