#' @title
#' NA preprocessing
#'
#' @description
#' Sometimes `na.omit` cannot be used with data set because it will end up with
#' too few rows to do anything sensible. This can be relaxed. In practice it is
#' the `NA_integer_` and `NA_real_` that have to be omitted, but `NA_character_` can
#' be retained: just add `NA` as a factor level. To achieve this, you need to loop
#' through variables in your data frame:
#'
#' * if a variable `x` is already a factor and `anyNA(x)` is `TRUE`, do `x <- addNA(x)`.
#'   The "and" is important. If `x` has no `NA`, `addNA(x)` will add an unused `<NA>`
#'   level.
#'
#' * if a variable `x` is a character, do `x <- factor(x, exclude = NULL)` to
#'   coerce it to a factor. `exclude = NULL` will retain `<NA>` as a level.
#'
#' * if `x` is "logical", "numeric", "raw" or "complex", nothing should be
#'   changed. `NA` is just `NA`.
#'
#' `<NA>` factor level will not be dropped by `droplevels` or `na.omit`, and it is
#' valid for building a model matrix. Check the following examples.
#'
#' Once you add `NA` as a level in a factor / character, your dataset might
#' suddenly have more complete cases. Then you can run your model. If you still
#' get a "contrasts error", use `debug_contr_error2` to see what has happened.
#'
#' @param dat The full data set
#'
#' @importFrom tibble as_tibble
#'
#' @return
#' a data frame, with NA added as a level for factor / character.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(y = 1:5,
#'                   x = factor(c(letters[1:4], NA)),
#'                   z = c(letters[1:4], NA))
#' dat
#' na_preprocessing(dat)
#'
#' na.omit(dat)
#' na.omit(na_preprocessing(dat))

na_preprocessing <- function(dat) {

  for (j in 1:ncol(dat)) {
    x <- dat[[j]]
    if (is.factor(x) && anyNA(x)) dat[[j]] <- base::addNA(x)
    if (is.character(x)) dat[[j]] <- factor(x, exclude = NULL)
    }

  tibble::as_tibble(dat)

}




