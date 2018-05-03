

#' Calculate difference between two dates
#'
#' Calculates the difference between two dates, in whole or fractional, years,
#' months, weeks, days, hours, minutes, or seconds. Really just a convenience
#' wrapper for lubridate functions
#'
#' /url{http://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/}
#' was a big help in understanding lubridate package.
#'
#' @param start The starting date.
#' @param end The ending date.
#' @param units Units: "years", "months", "weeks", "days", "hours", "minutes", "seconds". Default is "days".
#' @param precise TRUE (default) for fractional diff or FALSE for integer (not rounded) diff.
#'
#' @return A numeric object or vector
#' @export
#'
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom lubridate dyears
#' @importFrom lubridate dweeks
#' @importFrom lubridate ddays
#' @importFrom lubridate dhours
#' @importFrom lubridate dminutes
#' @importFrom lubridate as.duration
#' @importFrom lubridate interval
#'
#' @examples
#' library(dplyr)
#'
#' start <- c("2012-08-21", "1995-09-01", "1984-08-15", "1976-11-15", "2018-04-20")
#' end <- c("2012-09-16", "2012-09-06", "2012-08-22", "2018-04-20", "1976-11-15")
#'
#' df <- tibble::tibble(start = start,
#'                      end = end)
#'
#' calc_date_diff(start = start, end = end)
#' calc_date_diff(start = start, end = end, units = "years")
#' calc_date_diff(start = start, end = end, units = "months")
#' calc_date_diff(start = start, end = end, units = "weeks")
#' calc_date_diff(start = start, end = end, units = "days")
#' calc_date_diff(start = start, end = end, units = "hours")
#' calc_date_diff(start = start, end = end, units = "minutes")
#' calc_date_diff(start = start, end = end, units = "seconds")
#'
#' df %>% dplyr::mutate(diff_yrs = calc_date_diff(start, end, units = "years"),
#'               diff_mos = calc_date_diff(start, end, units = "months"),
#'               diff_wks = calc_date_diff(start, end, units = "weeks"),
#'               diff_dys = calc_date_diff(start, end, units = "days"),
#'               diff_hrs = calc_date_diff(start, end, units = "hours"),
#'               diff_mns = calc_date_diff(start, end, units = "minutes"),
#'               diff_scs = calc_date_diff(start, end, units = "seconds")
#' )
#'
#' df %>% dplyr::mutate(diff_yrs = calc_date_diff(start, end, units = "years",
#'                                         precise = FALSE),
#'               diff_mos = calc_date_diff(start, end, units = "months",
#'                                         precise = FALSE),
#'               diff_wks = calc_date_diff(start, end, units = "weeks",
#'                                         precise = FALSE),
#'               diff_dys = calc_date_diff(start, end, units = "days",
#'                                         precise = FALSE),
#'               diff_hrs = calc_date_diff(start, end, units = "hours",
#'                                         precise = FALSE),
#'               diff_mns = calc_date_diff(start, end, units = "minutes",
#'                                         precise = FALSE),
#'               diff_scs = calc_date_diff(start, end, units = "seconds",
#'                                         precise = FALSE)
#' )
#'
calc_date_diff <- function(start,
                           end = Sys.Date(),
                           units = "days",
                           precise = TRUE) {

  if (any(!inherits(start,
                    c("POSIXt", "POSIXct", "POSIXlt", "Date", "character"))))
    stop("date(s) not in POSIXt or Date or character format")

  if (any(!inherits(end,
                    c("POSIXt", "POSIXct", "POSIXlt", "Date", "character"))))
    stop("date(s) not in POSIXt or Date or character format")

  denom <- dplyr::case_when(
    units == "years" ~ lubridate::dyears(1),
    units == "months" ~ lubridate::dyears(1) / 12,
    units == "weeks" ~ lubridate::dweeks(1),
    units == "days" ~ lubridate::ddays(1),
    units == "hours" ~ lubridate::dhours(1),
    units == "minutes" ~ lubridate::dminutes(1),
    units == "seconds" ~ 1
  )

  if (precise == TRUE) {
    lubridate::as.duration(
      lubridate::interval(start = start, end = end)) / denom
  } else {
    lubridate::as.duration(
      lubridate::interval(start = start, end = end)) %/% denom
  }
}
