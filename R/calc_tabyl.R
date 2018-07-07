
#' Make a tabyl with totals and formatting
#'
#' This is a simple wrapper around the tabyl function from the janitor
#' package. So rather than writing a bunch of lines over and over again, I
#' made this shortened function to make my life easier. Since the first
#' argument takes a data frame or a tibble, this function is pipeable.
#'
#' @param data A data frame or tibble.
#' @param x A single variable that you want to see the formatted tabyl for.
#'
#' @return Returns a data.frame with frequencies and percentages of the
#'   tabulated variable(s).
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
#' calc_tabyl(data = iris, x = Species)
#'
calc_tabyl <- function(data, x) {

  # stopifnot(class(df) %in% c("tbl_df", "tbl", "data.frame"))
  if (is.list(data) && !"data.frame" %in% class(data)) {
    stop("tabyl() is meant to be called on vectors and data.frames; convert non-data.frame lists to one of these types")
  }

  x_enq <- rlang::enquo(x)

  data %>%
    janitor::tabyl(!! x_enq) %>%
    janitor::adorn_totals(where = "row") %>%
    janitor::adorn_pct_formatting()

}


