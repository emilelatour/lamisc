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
#' @param bootstrap_options A character vector for bootstrap table options.
#'   Please see package vignette or visit the w3schools' Bootstrap Page for more
#'   information. Possible options include basic, striped, bordered, hover,
#'   condensed, responsive and none.
#' @param escape Boolean; whether to escape special characters when producing
#'   HTML or LaTeX tables. When escape = FALSE, you have to make sure that
#'   special characters will not trigger syntax errors in LaTeX or HTML.
#' @param align Column alignment: a character vector consisting of 'l' (left),
#'   'c' (center) and/or 'r' (right). By default or if align = NULL, numeric
#'   columns are right-aligned, and other columns are left-aligned. If
#'   length(align) == 1L, the string will be expanded to a vector of individual
#'   letters, e.g. 'clc' becomes c('c', 'l', 'c'), unless the output format is
#'   LaTeX.
#' @param caption The table caption.
#' @param cols A numeric value or vector indicating which column(s) to be selected.
#' @param widths A character string telling HTML & LaTeX how wide the column
#'   needs to be, e.g. "10cm", "3in" or "30em".
#' @param full_width A TRUE or FALSE variable controlling whether the HTML table
#'   should have 100\ the preferable format for full_width. If not specified, a
#'   HTML table will have full width by default but this option will be set to
#'   FALSE for a LaTeX table
#' @param position A character string determining how to position the table on a
#'   page. Possible values include left, center, right, float_left and
#'   float_right. Please see the package doc site for demonstrations. For a
#'   LaTeX table, if float_* is selected, LaTeX package wrapfig will be
#'   imported.
#' @param font_size A numeric input for table font size
#' @param row_label_position A character string determining the justification of
#'   the row labels in a table. Possible values inclued l for left, c for
#'   center, and r for right. The default value is l for left justifcation.
#' @param stripe_color LaTeX option allowing users to pick a different color for
#'   their strip lines. This option is not available in HTML
#' @param stripe_index LaTeX option allowing users to customize which rows
#'   should have stripe color.
#' @param ... Other arguments passed to knitr::kable (see Examples and
#'   References).
#'
#'
#' @import dplyr
#' @importFrom janitor tabyl
#' @importFrom janitor adorn_totals
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra add_header_above
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
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
                                  format = "html",
                                  full_width = FALSE,
                                  bootstrap_options = c("basic"),
                                  escape = TRUE,
                                  align = "lccc",
                                  caption = NULL,
                                  cols = NULL,
                                  widths = NULL,
                                  position = "center",
                                  font_size = NULL,
                                  row_label_position = "l",
                                  stripe_color = "gray!6",
                                  stripe_index = NULL,
                                  ...) {

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  if (is.null(x_name)) {
    x_name <- rlang::quo_name(x)
  }

  if (is.null(y_name)) {
    y_name <- rlang::quo_name(y)
  }

  header_df <- tibble::tibble(header_names = c(" ", y_name, " "),
                              col_span = c(1, 2, 1))

  data %>%
    janitor::tabyl(!! x, !! y) %>%
    janitor::adorn_totals(where = "row") %>%
    janitor::adorn_totals(where = "col") %>%
    dplyr::rename(
      # new = old
      !! x_name := names(.)[1]) %>%
    knitr::kable(.,
                 format = format,
                 escape = escape,
                 align = align,
                 caption = caption,
                 ...) %>%
    kableExtra::kable_styling(full_width = full_width,
                              bootstrap_options = bootstrap_options,
                              position = position,
                              font_size = font_size,
                              row_label_position = row_label_position,
                              stripe_color = stripe_color,
                              stripe_index = stripe_index) %>%
    kableExtra::column_spec(1, bold = TRUE) %>%
    kableExtra::add_header_above(header = header_df)


}
