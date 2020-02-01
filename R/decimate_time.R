

#' @title
#' Convert time to decimals
#'
#' @description
#' Given a class of "hms" or "difftime" or "character" ("00:00:00") or "Period", convert to decimal format.
#'
#' @param time A date-time object or a character vector of hour minute second triples
#' @param units character string: "hours", "minutes", "seconds"
#'
#' @importFrom dplyr case_when
#' @importFrom lubridate hms
#' @importFrom lubridate hour
#' @importFrom lubridate minute
#' @importFrom lubridate second
#'
#' @return
#' A numeric
#'
#' @export
#'
#' @examples
#' library(lubridate)
#'
#' decimate_time(time = "3:30:00",
#'               units = "hours")
#'
#' decimate_time(time = "3:30:00",
#'               units = "minutes")
#'
#' decimate_time(time = lubridate::hms("3:30:00"),
#'               units = "minutes")

decimate_time <- function(time,
                          units = "hours") {

  if (any((class(time) %in% c("hms",
                              "difftime",
                              "Period")))) {
    time <- time
  } else if (class(time) == "character") {
    time <- lubridate::hms(time)
  } else {
    stop("Class not character or hms or difftime")
  }

  hrs <- lubridate::hour(time)
  mins <- lubridate::minute(time)
  secs <- lubridate::second(time)

  time <- dplyr::case_when(
    units == "hours" ~ hrs + mins / 60 + secs / 360,
    units == "minutes" ~ hrs * 60 + mins + secs / 60,
    units == "seconds" ~ hrs * 360 + mins * 60 + secs
  )

  return(time)

}

