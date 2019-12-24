#' @title
#' Replaces blank-ish elements of a factor or character vector to NA
#'
#' @description
#' Replaces blank-ish elements of a factor or character vector to NA
#'
#' @param x a vector of factor or character or any type
#' @param na_strings case sensitive strings that will be coverted to NA. The
#'   function will do a trimws(x,'both') before conversion. If NULL, do only
#'   trimws, no conversion to NA.
#'
#' @return Returns a vector trimws (always for factor, character) and NA
#'   converted (if matching na_strings). Attributes will also be kept
#'   ('label','labels', 'value.labels').
#'
#' @export
#'
#' @references
#' https://stackoverflow.com/questions/24172111/change-the-blank-cells-to-na
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' blank_to_na(x = c("NA", "na", 1, 2, 3))
#'
#' df <- tibble::tibble(
#'   a = c("NA", "na", 1, 2, 3),
#'   b = factor(c("NA", "na", 1, 2, 3))
#' )
#' df
#'
#' levels(df$b)
#'
#' df2 <- df %>%
#'   mutate(a = blank_to_na(a),
#'          b = blank_to_na(b))
#'
#' df2
#' levels(df2$b)
blank_to_na <- function(x, na_strings = c("",
                                          ".",
                                          "NA",
                                          "na",
                                          "N/A",
                                          "n/a",
                                          "NaN",
                                          "nan")) {

  ## Don't do anything if not a character or factor ----------------

  if (!is.factor(x) & !is.character(x)) {
    return(x)
  }


  ## Get factor levels if applicable ----------------

  x_lvls <- NULL

  if (is.factor(x)) {
    x_lvls <- levels(x)
  }


  ## Store attributes of x for later ----------------

  x_label <- attr(x, "label", exact = TRUE)
  x_labels <- attr(x, "labels", exact = TRUE)
  x_value_labels <- attr(x, "value.labels", exact = TRUE)


  ## Trim whitespace from x and the attributes ----------------

  x <- trimws(x, which = "both")  # trimws will convert factor to character

  if (!is.null(x_label)) {
    x_label <- trimws(x_label, which = "both")
  }

  if (!is.null(x_labels)) {
    x_labels <- trimws(x_labels, which = "both")
  }

  if (!is.null(x_value_labels)) {
    x_value_labels <- trimws(x_value_labels, which = "both")
  }

  if (!is.null(x_lvls)) {
    x_lvls <- trimws(x_lvls, which = "both")
  }

  ## Convert to NA ----------------

  if (!is.null(na_strings)) {

    x[x %in% na_strings] <- NA

    # remove na_strings from value labels
    x_labels <- x_labels[!x_labels %in% na_strings]
    x_value_labels <- x_value_labels[!x_value_labels %in% na_strings]

  }

  if (!is.null(x_lvls) & !is.null(na_strings)) {
    x_lvls <- x_lvls[!x_lvls %in% na_strings]
  }


  ## Reset factor levels ----------------

  if (!is.null(x_lvls)) {
    x <- factor(x,
                levels = x_lvls)
  }


  ## Reapply labels ----------------

  if (!is.null(x_label)) {
    attr(x, "label") <- x_label
  }

  if (!is.null(x_labels)) {
    attr(x, "labels") <- x_labels
  }

  if (!is.null(x_value_labels)) {
    attr(x, "value.labels") <- x_value_labels
  }


  ## End function ----------------

  return(x)

}
