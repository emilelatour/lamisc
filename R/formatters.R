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
#' The functions were adapted from the `scales` package, with two deliberate
#' differences: `accuracy` must be a single value (vectorised, per-element
#' accuracy is not supported) and is validated rather than allowed to produce
#' `NA`.
#'
#' @details
#' When `as_numeric = TRUE` the rounded value is returned directly, without a
#' round trip through [base::format()]. Two consequences:
#'
#' * The value is returned on the **scaled** basis, so
#'   `fmt_pct(0.5, as_numeric = TRUE)` returns `50`, matching what would have
#'   been displayed.
#' * `prefix`, `suffix`, `big.mark`, `decimal.mark`, and `trim` are ignored,
#'   since they only affect the character representation.
#'
#' `as_numeric` and `as_factor` are mutually exclusive.
#'
#' @return A character vector the same length as `x`; a numeric vector if
#'   `as_numeric = TRUE`, or a factor if `as_factor = TRUE`.
#' @param x A numeric vector to format.
#' @param accuracy A single positive number to round to, or `NULL` to guess
#'   from the range of `x`. Vectors of length greater than one are an error.
#' @param scale A scaling factor: `x` will be multiply by `scale` before
#'   formating (useful if the underlying data is on another scale,
#'   e.g. for computing percentages or thousands).
#' @param prefix,suffix Symbols to display before and after value. Ignored
#'   when `as_numeric = TRUE`.
#' @param big.mark Character used between every 3 digits to separate thousands.
#'   Ignored when `as_numeric = TRUE`.
#' @param decimal.mark The character to be used to indicate the numeric
#'   decimal point. Ignored when `as_numeric = TRUE`.
#' @param trim Logical, if `FALSE`, values are right-justified to a common
#'   width (see [base::format()]). Ignored when `as_numeric = TRUE`.
#' @param as_numeric Logical; if `TRUE`, the rounded value is returned as a
#'   numeric vector on the scaled basis. Cannot be combined with `as_factor`.
#' @param as_factor Logical; if `TRUE`, a factor is returned, with levels
#'   ordered by the underlying numeric value. Cannot be combined with
#'   `as_numeric`.
#' @param ... Other arguments passed on to [base::format()]. Note that
#'   [base::format()] does not reject unknown arguments, so a misspelled
#'   argument name will be silently ignored.
#'
#' @importFrom forcats fct_reorder
#' @importFrom readr parse_number
#'
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
#'
#' # Numeric return is on the scaled basis
#' fmt_num(12345.789, accuracy = .1, as_numeric = TRUE)
#' fmt_pct(0.5, as_numeric = TRUE)

fmt_num <- function(x,
                    accuracy = 1,
                    scale = 1,
                    prefix = "",
                    suffix = "",
                    big.mark = "",
                    decimal.mark = ".",
                    trim = TRUE,
                    as_numeric = FALSE,
                    as_factor = FALSE, ...) {

  if (as_numeric && as_factor) {
    stop("`as_numeric` and `as_factor` cannot both be TRUE.", call. = FALSE)
  }

  if (!is.null(accuracy)) .check_accuracy(accuracy)

  if (length(x) == 0) {
    return(if (as_numeric) numeric() else if (as_factor) factor() else character())
  }

  accuracy <- accuracy %||% .precision(x)

  x <- .round_any_numeric(x, accuracy / scale)

  ## Numeric return -- no round trip through format() ----------------
  if (as_numeric) return(scale * x)

  nsmall <- -floor(log10(accuracy))
  nsmall <- min(max(nsmall, 0), 20)

  res <- format(
    scale * x,
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    trim = trim,
    nsmall = nsmall,
    scientific = FALSE,
    ...
  )
  res <- paste0(prefix, res, suffix)
  res[is.infinite(x)] <- as.character(x[is.infinite(x)])

  # restore NAs from input vector
  res[is.na(x)] <- NA

  if (as_factor) {
    forcats::fct_reorder(.f = res,
                         .x = readr::parse_number(res),
                         .na_rm = FALSE)
  } else {
    res
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

## check_accuracy ----------------

.check_accuracy <- function(accuracy, max = Inf) {
  if (!is.numeric(accuracy) ||
      length(accuracy) != 1L ||
      !is.finite(accuracy) ||
      accuracy <= 0 ||
      accuracy >= max) {
    stop("`accuracy` must be a single positive number",
         if (is.finite(max)) paste0(" less than ", max), ".",
         call. = FALSE)
  }
  invisible(accuracy)
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
#' Formatter for p-values, substituting a bound for values too small or too
#' large to display at the requested `accuracy`. Values below `accuracy` are
#' shown as `< accuracy` (e.g. `< 0.001`); values above `1 - accuracy` are
#' shown as `> 1 - accuracy` (e.g. `> 0.999`) unless `allow_one = TRUE`.
#' Adapted from the `scales` package.
#'
#' @return A character vector the same length as `x`, or a factor if
#'   `as_factor = TRUE`.
#' @param x A numeric vector of p-values.
#' @param accuracy A single number greater than 0 and less than 1 to round to.
#'   Unlike [fmt_num()], `NULL` is not accepted, since guessing precision from
#'   the observed p-values is not meaningful.
#' @param decimal.mark The character to be used to indicate the numeric
#'   decimal point.
#' @param add_p Add "P =" before the value?
#' @param p_lbl Character to use for "P = ". Default, uppercase "P".
#' @param allow_one Logical; if `TRUE`, p-values at or near 1 are printed as-is
#'   (e.g. `1.000`) rather than as `> 1 - accuracy` (e.g. `> 0.999`).
#' @param as_factor Logical; if `TRUE`, a factor is returned, with levels
#'   ordered by the underlying numeric value.
#'
#' @section Caveats:
#' `x` is not checked against the range \[0, 1\]; an out-of-range value will be
#' formatted as if it were a valid p-value.
#'
#' `NA` values are returned as `NA`, except when `add_p = TRUE`, where they are
#' rendered as `"P = NA"` so that the label is present in every cell of a
#' results column. Combining `add_p = TRUE` with `as_factor = TRUE` therefore
#' keeps `"P = NA"` as a factor level and emits a parsing warning; use one or
#' the other.
#'
#' With `as_factor = TRUE`, a bound such as `"> 0.999"` and a literal `"0.999"`
#' parse to the same numeric value and may be ordered arbitrarily with respect
#' to each other.
#'
#' @export
#' @examples
#' p <- c(.50, 0.12, .045, .011, .009, .00002, NA)
#' fmt_pvl(p)
#' fmt_pvl(p, accuracy = .01)
#' fmt_pvl(p, add_p = TRUE)
#'
#' # Upper bound and allow_one
#' q <- c(0.0002, 0.045, 0.9996, 1.000, NA)
#' fmt_pvl(q)
#' fmt_pvl(q, allow_one = TRUE)
#' fmt_pvl(q, accuracy = .1)
#' fmt_pvl(q, accuracy = .1, allow_one = TRUE)
fmt_pvl <- function(x,
                    accuracy = .001,
                    decimal.mark = ".",
                    add_p = FALSE,
                    p_lbl = "P",
                    allow_one = FALSE,
                    as_factor = FALSE) {

  .check_accuracy(accuracy, max = 1)

  res <- fmt_num(
    x,
    accuracy = accuracy,
    decimal.mark = decimal.mark,
    big.mark = ""
  )

  if (add_p) res <- paste0(p_lbl, " = ", res)
  # if (add_p) res[!is.na(res)] <- paste0(p_lbl, " = ", res[!is.na(res)])

  ## Lower bound ----------------
  below <- fmt_num(accuracy,
                   accuracy = accuracy,
                   decimal.mark = decimal.mark,
                   big.mark = "")

  below <- if (add_p) paste0(p_lbl, " < ", below) else paste0("< ", below)

  res[!is.na(x) & x < accuracy] <- below

  ## Upper bound ----------------
  if (!allow_one) {
    cutoff <- 1 - accuracy

    above <- fmt_num(cutoff,
                     accuracy = accuracy,
                     decimal.mark = decimal.mark,
                     big.mark = "")

    above <- if (add_p) paste0(p_lbl, " > ", above) else paste0("> ", above)

    res[!is.na(x) & x > cutoff] <- above
  }

  if (as_factor) {
    forcats::fct_reorder(.f = res,
                         .x = readr::parse_number(res),
                         .na_rm = FALSE)
  } else {
    res
  }

}
