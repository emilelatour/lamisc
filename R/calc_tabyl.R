
#' calc_tabyl
#'
#' This is a simple wrapper around the tabyl function from the janitor
#' package. So rather than writing a bunch of lines over and over again, I
#' made this shortened function to make my life easier. Since the first
#' argument takes a data frame or a tibble, this function is pipeable.
#'
#' @param df A data frame or tibble.
#' @param x A single variable that you want to see the formatted tabyl for.
#'
#' @return
#' @export
#'
#' @import rlang
#' @import janitor
#' @import dplyr
#'
#' @examples
#' iris %>%
#'   calc_tabyl(Species)
#'
#' calc_tabyl(df = iris, x = Species)
#'
calc_tabyl <- function(df, x) {

  x_enq <- rlang::enquo(x)

  df %>%
    janitor::tabyl(!! x_enq) %>%
    janitor::adorn_totals(where = "row") %>%
    janitor::adorn_pct_formatting()

}


