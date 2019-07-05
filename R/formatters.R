#' @title
#' Number formatters
#'
#' @description
#' A set of functions to format numeric values:
#'
#' * `fmt_num()` is a generic formatter for numbers.
#' * `fmt_pct()` multiply values by one hundred and display percent sign.
#'
#' All formatters allow you to re-`scale` (multiplicatively), to round to
#' specified `accuracy`, to add custom `suffix` and `prefix` and to specify
#' `decimal.mark` and `big.mark`.
#'
#' The functions were adapted directly from the `scales` package.
#'
#' @return `*_format()` returns a function with single parameter
#'   `x`, a numeric vector, that returns a character vector.
#' @param x A numeric vector to format.
#' @param accuracy Number to round to, `NULL` for automatic guess.
#' @param scale A scaling factor: `x` will be multiply by `scale` before
#'   formating (useful if the underlying data is on another scale,
#'   e.g. for computing percentages or thousands).
#' @param prefix,suffix Symbols to display before and after value.
#' @param big.mark Character used between every 3 digits to separate thousands.
#' @param decimal.mark The character to be used to indicate the numeric
#'   decimal point.
#' @param trim Logical, if `FALSE`, values are right-justified to a common
#'   width (see [base::format()]).
#' @param as_numeric Logical; if TRUE, a numeric value is returned
#' @param ... Other arguments passed on to [base::format()].
#' @export
#' @examples
#' v <- c(12.3, 4, 12345.789, 0.0002)
#' fmt_num(v)
#' fmt_num(v, big.mark = ",")
#' fmt_num(v, accuracy = .001)
#' fmt_num(v, accuracy = .001, decimal.mark = ",")
#' fmt_num(v, accuracy = .5)
#'
#' w <- c(-90.256, -0.1212, NA, NA)
#' fmt_num(w)
#' fmt_num(w, big.mark = ",")
#' fmt_num(w, accuracy = .001)
#' fmt_num(w, accuracy = .001, decimal.mark = ",")
#' fmt_num(w, accuracy = .5)

fmt_num <- function(x,
                    accuracy = 1,
                    scale = 1,
                    prefix = "",
                    suffix = "",
                    big.mark = "",
                    decimal.mark = ".",
                    trim = TRUE,
                    as_numeric = FALSE, ...) {

  if (length(x) == 0) return(character())
  accuracy <- accuracy %||% .precision(x)
  x <- .round_any_numeric(x, accuracy / scale)
  nsmall <- -floor(log10(accuracy))
  nsmall <- min(max(nsmall, 0), 20)

  ret <- format(
    scale * x,
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    trim = trim,
    nsmall = nsmall,
    scientific = FALSE,
    ...
  )
  ret <- paste0(prefix, ret, suffix)
  ret[is.infinite(x)] <- as.character(x[is.infinite(x)])

  # restore NAs from input vector
  ret[is.na(x)] <- NA

  if (as_numeric) {
    as.numeric(ret)
  } else {
    ret
  }

}



#### Some helpers --------------------------------

## round_any_numeric ----------------

.round_any_numeric <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

## precision ----------------

.precision <- function(x) {
  # cannot calculate a precision if all values are Inf or NA
  if (all(is.infinite(x) | is.na(x))) {
    return(1)
  }

  rng <- range(x, na.rm = TRUE, finite = TRUE)

  span <- if (.zero_range(rng)) abs(rng[1]) else diff(rng)
  if (span == 0) {
    return(1)
  }

  10^floor(log10(span))
}

## zero_range ----------------

.zero_range <- function (x, tol = 1000 * .Machine$double.eps)
{
  if (length(x) == 1)
    return(TRUE)
  if (length(x) != 2)
    stop("x must be length 1 or 2")
  if (any(is.na(x)))
    return(NA)
  if (x[1] == x[2])
    return(TRUE)
  if (all(is.infinite(x)))
    return(FALSE)
  m <- min(abs(x))
  if (m == 0)
    return(FALSE)
  abs((x[1] - x[2])/m) < tol
}


#' @export
#' @rdname fmt_num
#' @examples
#' fmt_pct(x = runif(10))
#' fmt_pct(x = c(1, runif(10)), trim = FALSE)
fmt_pct <- function(x,
                    accuracy = NULL,
                     scale = 100,
                     prefix = "",
                     suffix = "%",
                     big.mark = "",
                     decimal.mark = ".",
                     trim = TRUE, ...) {
  fmt_num(
    x = x,
    accuracy = accuracy,
    scale = scale,
    prefix = prefix,
    suffix = suffix,
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    trim = trim,
    ...
  )
}


#' @title
#' p-values formatter
#'
#' @description
#' Formatter for p-values, adding a symbol "<" for small p-values. Adapted
#' directly from `scales` package.
#'
#' @return `pvalue_format` returns a function with single parameter
#'   `x`, a numeric vector, that returns a character vector.
#' @param accuracy Number to round to.
#' @param decimal.mark The character to be used to indicate the numeric
#'   decimal point.
#' @param add_p Add "P =" before the value?
#' @param x A numeric vector of p-values.
#' @export
#' @examples
#' p <- c(.50, 0.12, .045, .011, .009, .00002, NA)
#' fmt_pvl(p)
#' fmt_pvl(p, accuracy = .01)
#' fmt_pvl(p, add_p = TRUE)
fmt_pvl <- function(x,
                    accuracy = .001,
                    decimal.mark = ".",
                    add_p = FALSE) {
  res <- fmt_num(
    x,
    accuracy = accuracy,
    decimal.mark = decimal.mark,
    big.mark = ""
  )

  if (add_p) res <- paste0("P = ", res)

  below <- fmt_num(accuracy,
                  accuracy = accuracy,
                  decimal.mark = decimal.mark,
                  big.mark = "")

  if (add_p) {
    below <- paste0("P < ", below)
  } else {
    below <- paste0("< ", below)
  }

  res[x < accuracy] <- below
  res
}
