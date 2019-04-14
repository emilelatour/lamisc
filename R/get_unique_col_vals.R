

#' @title
#' Extract the unique, non-duplicate values in a column
#'
#' @description
#' Given a data frame and a column, the function will return the unique values
#' of that column. Useful when making factors.
#'
#' @import rlang
#' @import dplyr
#' @importFrom forcats fct_unique
#' @importFrom forcats fct_drop
#'
#' @param data A data frame or tibble
#' @param x Column in the data
#'
#' @return A character vector or `NULL`
#' @export
#'
#' @examples
#' get_unique_col_vals(data = iris,
#'                     x = Species)
#'
#' iris2 <- subset(iris, Species != "setosa")
#' table(iris2$Species)
#' get_unique_col_vals(data = iris2,
#'                     x = Species)
#'
#' get_unique_col_vals(data = mtcars, x = gear)

get_unique_col_vals <- function(data, x) {

  x_enq <- rlang::enquo(x)

  data %>%
    dplyr::pull(!! x_enq) %>%
    as.character(.) %>%
    forcats::fct_drop(.) %>%
    forcats::fct_unique(.) %>%
    as.character(.)

}

