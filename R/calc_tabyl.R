
#' @title
#' Make a tabyl with totals and formatting
#'
#' @description
#' This is a simple wrapper around the `tabyl` function from the janitor
#' package. So rather than writing a bunch of lines over and over again, I
#' made this shortened function to make my life easier. Since the first
#' argument takes a data frame or a tibble, this function is pipeable.
#'
#' @param data A data frame or tibble.
#' @param ... the column arguments to `janitor::tabyl`: `var1` the column name
#'   of the first variable, `var2` (optional) the column name of the second
#'   variable (the rows in a 2-way tabulation), `var3` (optional) the column
#'   name of the third variable (the list in a 3-way tabulation).
#' @param show_na should counts of \code{NA} values be displayed?  In a one-way
#'   tabyl, the presence of \code{NA} values triggers an additional column
#'   showing valid percentages(calculated excluding \code{NA} values).
#' @param show_missing_levels should counts of missing levels of factors be
#'   displayed?  These will be rows and/or columns of zeroes.  Useful for
#'   keeping consistent output dimensions even when certain factor levels may
#'   not be present in the data.
#' @return Returns a data.frame with frequencies and percentages of the
#'   tabulated variable(s).  A 3-way tabulation returns a list of data.frames.
#'
#' @export
#'
#' @import janitor
#' @import dplyr
#'
#' @examples
#'
#' calc_tabyl(mtcars, cyl)
#' calc_tabyl(mtcars, cyl, gear)
#' calc_tabyl(mtcars, cyl, gear, am)
#'
#' # or using the %>% pipe
#' mtcars %>%
#'   calc_tabyl(cyl, gear)
#'
#' # illustrating show_na functionality:
#' my_cars <- rbind(mtcars, rep(NA, 11))
#' my_cars %>% calc_tabyl(cyl)
#' my_cars %>% calc_tabyl(cyl, show_na = FALSE)
#'
#' # Calling on a single vector not in a data.frame:
#' val <- c("hi", "med", "med", "lo")
#' calc_tabyl(val)
calc_tabyl <- function(data, ..., show_na = TRUE, show_missing_levels = TRUE) {

  # stopifnot(class(df) %in% c("tbl_df", "tbl", "data.frame"))
  if (is.list(data) && !"data.frame" %in% class(data)) {
    stop("tabyl() is meant to be called on vectors and data.frames; convert non-data.frame lists to one of these types")
  }

  data %>%
    janitor::tabyl(dat = ., ..., show_na = TRUE, show_missing_levels = TRUE) %>%
    janitor::adorn_totals(where = "row") %>%
    janitor::adorn_pct_formatting()

}

