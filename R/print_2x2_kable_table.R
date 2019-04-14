#' @title
#' Print a 2x2 table with kable
#'
#' @description
#' In my work with agreement statistics, I want to print a nice looking 2x2
#' table in the documents that I share with reaserchers. This function is a
#' wrapper for the steps that I take to make these types of 2x2 tables.
#'
#' @param data A data frame or tibble
#' @param x "X" variable; counts appear in the rows of the table
#' @param y "Y" variable; counts appear in the columns of the table
#' @param x_name A string; name/label for the "X" variable
#' @param y_name A string; name/label for the "Y" variable
#' @param format A character string. Possible values are "latex", "html",
#'   "markdown", "pandoc", and "rst". This is an argument for `knitr::kable()`
#'
#' @import dplyr
#' @import rlang
#' @importFrom janitor tabyl
#' @importFrom janitor adorn_totals
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra add_header_above
#'
#' @return A character vector of the table source code.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(forcats)
#' library(janitor)
#'
#' # Remove levels of some variables from the diamonds data frame so that it
#' # can be used as an example of a 2x2 table.
#' foo <- diamonds %>%
#'   dplyr::filter(color %in% c("E", "D")) %>%
#'   dplyr::filter(cut %in% c("Fair", "Good")) %>%
#'   dplyr::mutate(color = forcats::fct_drop(color),
#'                 cut = forcats::fct_drop(cut))
#'
#' # Tables using janitor::tabyl() and base::table()
#' foo %>% janitor::tabyl(color, cut)
#' table(foo$color, foo$cut)
#'
#' # Example using the lamisc package
#' print_2x2_kable_table(data = foo,
#'                       x = color,
#'                       y = cut,
#'                       x_name = "COLOR",
#'                       y_name = "CUT")
#'
print_2x2_kable_table <- function(data, x, y,
                                  x_name = NULL,
                                  y_name = NULL,
                                  format = "html") {

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  header_names <- c(1, 2, 1)
  names(header_names) <- c(" ", as.name(paste(y_name)), " ")

  data %>%
    janitor::tabyl(!! x, !! y) %>%
    janitor::adorn_totals(where = "row") %>%
    janitor::adorn_totals(where = "col") %>%
    dplyr::rename(
      # new = old
      !! x_name := names(.)[1]) %>%
    knitr::kable(., format = format) %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::column_spec(1, bold = TRUE) %>%
    kableExtra::add_header_above(header_names)


}
