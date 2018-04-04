
#' Fix messy dates
#'
#' @param x
#' @param formats
#' @param date_system
#'
#' @return
#' @export
#'
#' @examples
#' pacman::p_load(
#' tidyverse,    # packages ggplot2, tibble, tidyr, readr, purrr, and dplyr
#' janitor,      # for working with dirty data
#' lubridate,    # for working with dates and times
#' magrittr       # includes the %<>% assignment-pipe (%>% is loaded from dplyr)
#' )
#'
#' bar <- tibble::tribble(
#'   ~date,                     ~comment,
#'   NA_character_,                      "",
#'   NA_character_,                      "",
#'   "12/21/2011",                       "",
#'   "2/1/2015",                         "",
#'   "5/17/12",                          "",
#'   "10/3/15",                          "",
#'   "42253",        "should be 2015-09-06",
#'   "42309",        "should be 2015-11-01",
#'   "5 /23/2015",                       "",
#'   "10/3 /2015",                       "",
#'   "unknown",    "Should turn into an NA",
#'   "10/5/10",      "should be Oct 5 2010",
#'   "2/1213",  "Can't handle this one yet",
#'   "21213",   "Can't handle this one yet"
#' )
#'
#'
#'
#' bar %<>%
#'   mutate(date2 = fix_messy_dates(date)
#'   )
#'
#' bar

fix_messy_dates <- function(x,
                            formats = c("m/d/y"),
                            date_system = "modern") {

  # Notes from janitor::excel_numeric_to_date
  # Converts numbers like 42370 into date values like 2016-01-01.
  # Defaults to the modern Excel date encoding system. However, Excel for
  # Mac 2008 and earlier Mac versions of Excel used a different date system.
  # To determine what platform to specify: if the date 2016-01-01 is
  # represented by the number 42370 in your spreadsheet, it's the modern
  # system. If it's 40908, it's the old Mac system.

  # date_system -- the date system, either "modern" or "mac pre-2011".

  suppressWarnings(
    dplyr::coalesce(
      as.Date(lubridate::parse_date_time(x, formats)),
      janitor::excel_numeric_to_date(as.numeric(x), date_system = date_system)
    )
  )

}


