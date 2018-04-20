
#' Consistent formatting of a column of dates
#'
#' It's come up a number of times, typically when reading in data from Excel,
#'   where a column of data that are supposed to be dates are loaded as a
#'   character column with a number of fomats. There's no one good solution to
#'   this but by combining functions lubridate::parse_date_time() and
#'   janitor::excel_numeric_to_date() I am able to fix this date issue when it
#'   comes up.
#'
#' From janitor::excel_numeric_to_date:
#'   Converts numbers like 42370 into date values like 2016-01-01.
#'   Defaults to the modern Excel date encoding system. However, Excel for
#'   Mac 2008 and earlier Mac versions of Excel used a different date system.
#'   To determine what platform to specify: if the date 2016-01-01 is
#'   represented by the number 42370 in your spreadsheet, it's the modern
#'   system. If it's 40908, it's the old Mac system.
#'
#' Note that NA's can be coerced in the conversion. This is expected and the
#'   warning message has been suppressed purposely. For this reason, it is
#'   important to spot check the data after using this function.
#'
#' @param x A character or numeric vector of dates
#' @param formats Date formats passed to lubridate::parse_date_time()
#' @param date_system Date system for janitor::excel_numeric_to_date(), either
#'     "modern" or "mac pre-2011"
#'
#' @return
#' @export
#'
#' @import dplyr
#' @importFrom janitor excel_numeric_to_date
#' @importFrom lubridate parse_date_time
#'
#'
#' @examples
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
#' bar %>%
#'   dplyr::mutate(date2 = fix_messy_dates(date))
#'

fix_messy_dates <- function(x,
                            formats = c("m/d/y"),
                            date_system = "modern") {

  base::suppressWarnings(
    dplyr::coalesce(
      base::as.Date(lubridate::parse_date_time(x, formats)),
      janitor::excel_numeric_to_date(base::as.numeric(x),
                                     date_system = date_system)
    )
  )

}

