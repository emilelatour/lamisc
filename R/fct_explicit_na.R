#' Explicitly Assign a Level for Missing Values in a Factor
#'
#' A function to handle missing values in a factor by explicitly assigning them a specified level.
#' This is based on a deprecated function in the \code{forcats} package (\code{forcats::fct_explicit_na}).
#'
#' @param f A factor (or character vector). If a character vector is provided, it will be automatically converted to a factor.
#' @param na_level A character string specifying the level to assign to missing values (NAs). Defaults to \code{"(Missing)"}.
#'
#' @importFrom forcats fct_expand
#' @importFrom forcats lvls_revalue
#'
#' @return A factor with missing values explicitly assigned to the specified \code{na_level}.
#'
#' @details
#' - If the factor contains missing values (NAs), they are replaced with the value specified by \code{na_level}.
#' - If the factor already has an NA level (i.e., a level with \code{NA} as its name), it will be replaced by \code{na_level}.
#' - If there are no missing values, the factor is returned unchanged.
#'
#' @examples
#' # Example 1: Factor with missing values
#' f <- factor(c("A", "B", NA, "C"))
#' fct_explicit_na(f)
#'
#' # Example 2: Specifying a custom level for missing values
#' fct_explicit_na(f, na_level = "Unknown")
#'
#' @seealso \code{\link[forcats]{fct_expand}}, \code{\link[forcats]{lvls_revalue}}
#'
#' @export

fct_explicit_na <- function(f,
                            na_level = "(Missing)") {

  f <- check_factor(f)
  is_missing <- is.na(f)
  is_missing_level <- is.na(levels(f))

  if (any(is_missing)) {

    f <- forcats::fct_expand(f, na_level)
    f[is_missing] <- na_level
    f

  } else if (any(is_missing_level)) {

    levs <- levels(f)
    levs[is.na(levs)] <- na_level
    forcats::lvls_revalue(f, levs)

  } else {

    f

  }

}



# check_factor <- function(x, arg = caller_arg(x), call = caller_env()) {
#   if (is.character(x)) {
#     factor(x)
#   } else if (is.factor(x)) {
#     x
#   } else {
#     cli::cli_abort(
#       "{.arg {arg}} must be a factor or character vector, not {.obj_type_friendly {x}}.",
#       call = call
#     )
#   }
# }
