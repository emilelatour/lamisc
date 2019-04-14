

#' @title
#' Get factor levels for a variable. Unique ones only or all of te
#'
#' @description
#' Given a data frame and a column, the function will return the factor levels.
#' If `unique_lvls = TRUE`, then it will only return the levels for which there
#' is a value in the data.
#'
#' @references
#' /url{https://chemicalstatistician.wordpress.com/2018/03/10/use-unique-instead-of-levels-to-find-the-possible-values-of-a-character-variable-in-r/}
#'
#' @import rlang
#' @import dplyr
#'
#' @param data A data frame or tibble
#' @param x Column in the data
#' @param unique_lvls Logical. If `TRUE` (default), then only the possible
#'   values of a factor are returned. If `FALSE`, then the original factor
#'   levels are returned.
#'
#' @return A character vector or `NULL`
#' @export
#'
#' @examples
#' get_fct_lvls(data = iris, x = Species)
#'
#' iris2 <- subset(iris, Species != "setosa")
#' table(iris2$Species)
#' get_fct_lvls(data = iris2, x = Species)
#' get_fct_lvls(data = iris2, x = Species, unique_lvls = FALSE)

get_fct_lvls <- function(data, x, unique_lvls = TRUE) {

  x_enq <- rlang::enquo(x)
  x_name <- rlang::quo_name(x_enq)

  if (!(any(class(data[[x_name]]) %in% c("factor", "ordered",
                                       "logical", "labelled")))) {
    stop("Variable must be a factor.")
                                       }

  if (unique_lvls == TRUE) {
    as.character(unique(data[[x_name]]))
  } else {
    levels(unique(data[[x_name]]))
  }

}



