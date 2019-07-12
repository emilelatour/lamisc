#' @title
#' Calculate difference between two dates
#'
#' @description
#' Calculates the difference between two dates, in whole or fractional, years,
#' months, weeks, days, hours, minutes, or seconds. Really just a convenience
#' wrapper for lubridate functions
#'
#'  Divide an interval by a period to determine its implied length in clock
#'  time. Periods track changes in clock times, which ignore time line
#'  irregularities.
#'
#'  Divide and interval by a duration to determine its physical length.
#'  Durations track the passage of physical time, which deviates from clock time
#'  when irregularities occur.
#'
#' @references
#'
#' \url{http://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/}
#'
#' \url{https://rdrr.io/cran/janitor/man/excel_numeric_to_date.html}
#'
#' \url{https://rdrr.io/cran/openxlsx/man/convertToDateTime.html}
#'
#' \url{https://www.rdocumentation.org/packages/lubridate/versions/1.7.4/topics/parse_date_time}
#'
#' @param start The starting date.
#' @param end The ending date.
#' @param units Units: "years", "months", "weeks", "days", "hours", "minutes",
#'   "seconds". Default is "days".
#' @param precise TRUE (default) for fractional diff or FALSE for integer (not
#'   rounded) diff.
#' @param length Options: "physical" or "implied". Per the lubridate cheat
#'   sheet, divide an interval by a __duration__ to determine its __physical__
#'   length; divide by a __period__ to determine its __implied__ length in clock
#'   time. The default is "implied" and this is what you would want to use most
#'   of the time when calculating age, time to event, etc.
#'
#' @usage
#' calc_date_diff(start,
#'                end = Sys.Date(),
#'                units = "days",
#'                precise = TRUE,
#'                length = "implied")
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
#' @importFrom lubridate dseconds
#' @importFrom lubridate as.duration
#' @importFrom lubridate interval
#' @importFrom lubridate years
#' @importFrom lubridate weeks
#' @importFrom lubridate days
#' @importFrom lubridate hours
#' @importFrom lubridate minutes
#' @importFrom lubridate seconds
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
#'                                                precise = FALSE),
#'                      diff_mos = calc_date_diff(start, end, units = "months",
#'                                                precise = FALSE),
#'                      diff_wks = calc_date_diff(start, end, units = "weeks",
#'                                                precise = FALSE),
#'                      diff_dys = calc_date_diff(start, end, units = "days",
#'                                                precise = FALSE),
#'                      diff_hrs = calc_date_diff(start, end, units = "hours",
#'                                                precise = FALSE),
#'                      diff_mns = calc_date_diff(start, end, units = "minutes",
#'                                                precise = FALSE),
#'                      diff_scs = calc_date_diff(start, end, units = "seconds",
#'                                                precise = FALSE)
#' )
#'
calc_date_diff <- function(start,
                           end = Sys.Date(),
                           units = "days",
                           precise = TRUE,
                           length = "implied") {

  if (any(!inherits(start,
                    c("POSIXt", "POSIXct", "POSIXlt", "Date", "character"))))
    stop("date(s) not in POSIXt or Date or character format")

  if (any(!inherits(end,
                    c("POSIXt", "POSIXct", "POSIXlt", "Date", "character"))))
    stop("date(s) not in POSIXt or Date or character format")

  if (length == "implied") {
    # divide an interval by a period to determine its implied length in clock
    # time. Periods track changes in clock times, which ignore time line
    # irregularities.
    denom <- dplyr::case_when(
      units == "years" ~ lubridate::years(1),
      units == "months" ~ months(1),
      units == "weeks" ~ lubridate::weeks(1),
      units == "days" ~ lubridate::days(1),
      units == "hours" ~ lubridate::hours(1),
      units == "minutes" ~ lubridate::minutes(1),
      units == "seconds" ~ lubridate::seconds(1))

  } else {
    # Divide and interval by a duration to determine its physical length.
    # Durations track the passage of physical time, which deviates from clock
    # time when irregularities occur.
    denom <- dplyr::case_when(
      units == "years" ~ lubridate::dyears(1),
      units == "months" ~ lubridate::dyears(1) / 12,
      units == "weeks" ~ lubridate::dweeks(1),
      units == "days" ~ lubridate::ddays(1),
      units == "hours" ~ lubridate::dhours(1),
      units == "minutes" ~ lubridate::dminutes(1),
      units == "seconds" ~ lubridate::dseconds(1))
  }

  if (precise == TRUE) {
    lubridate::interval(start = start, end = end) / denom
  } else {
    lubridate::interval(start = start, end = end) %/% denom
  }
}

