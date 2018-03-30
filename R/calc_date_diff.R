

#' calc_date_diff
#'
#' Adapted from eeptools::calc_age
#'
#' @param dob
#' @param enddate
#' @param units
#' @param precise
#'
#' @return
#' @export
#'
#' @examples
#' a <- as.Date(seq(as.POSIXct('1987-05-29 018:07:00'), len = 26, by = "21 day"))
#' b <- as.Date(seq(as.POSIXct('2002-05-29 018:07:00'), len = 26, by = "21 day"))
#' calc_date_diff(a, units = 'years', precise = FALSE)
#'
calc_date_diff <- function(dob,
                     enddate = Sys.Date(),
                     units = "months",
                     precise = TRUE) {

  if (!inherits(dob, "Date") | !inherits(enddate, "Date")) {
    stop("Both dob and enddate must be Date class objects")
  }
  if (any(enddate < dob)) {
    stop("End date must be a date after date of birth")
  }
  start <- as.POSIXlt(dob)
  end <- as.POSIXlt(enddate)
  if (precise) {
    start_is_leap <- ifelse(start$year %% 400 == 0,
                            TRUE,
                            ifelse(start$year %% 100 == 0,
                                   FALSE,
                                   ifelse(start$year %% 4 == 0,
                                          TRUE,
                                          FALSE)))
    end_is_leap <- ifelse(end$year %% 400 == 0,
                          TRUE,
                          ifelse(end$year %% 100 == 0,
                                 FALSE,
                                 ifelse(end$year %% 4 == 0,
                                        TRUE,
                                        FALSE)))
  }

  if (units == "days") {
    result <- difftime(end, start, units = "days")
  }
  else if (units == "months") {
    months <- sapply(mapply(seq,
                            as.POSIXct(start),
                            as.POSIXct(end),
                            by = "months",
                            SIMPLIFY = FALSE),
                     length) - 1
    if (precise) {
      month_length_end <-
        ifelse(end$mon == 1 & end_is_leap,
               29,
               ifelse(end$mon == 1,
                      28,
                      ifelse(end$mon %in% c(3, 5, 8, 10),
                             30,
                             31)))
      month_length_prior <-
        ifelse((end$mon - 1) == 1 & start_is_leap,
               29,
               ifelse((end$mon - 1) == 1,
                      28,
                      ifelse((end$mon - 1) %in% c(3, 5, 8, 10),
                             30,
                             31)))

      month_frac <-
        ifelse(end$mday > start$mday,
               (end$mday - start$mday) / month_length_end,
               ifelse(end$mday < start$mday,
                      (month_length_prior - start$mday) / month_length_prior +
                        end$mday / month_length_end,
                      0)
        )

      result <- months + month_frac
    }

    else {
      result <- months
    }
  }

  else if (units == "years") {
    years <- sapply(mapply(seq,
                           as.POSIXct(start),
                           as.POSIXct(end),
                           by = "years",
                           SIMPLIFY = FALSE),
                    length) - 1

    if (precise) {
      start_length <- ifelse(start_is_leap,
                             366,
                             365)
      end_length <- ifelse(end_is_leap,
                           366,
                           365)
      start_day <- ifelse(start_is_leap & start$yday >= 60,
                          start$yday - 1,
                          start$yday)
      end_day <- ifelse(end_is_leap & end$yday >= 60,
                        end$yday - 1,
                        end$yday)
      year_frac <-
        ifelse(start_day < end_day,
               (end_day - start_day) / end_length,
               ifelse(start_day > end_day,
                      (start_length - start_day) / start_length +
                        end_day / end_length,
                      0))

      result <- years + year_frac

    } else {
      result <- years
    }
  } else {
    stop("Unrecognized units. Please choose years, months, or days.")
  }
  return(result)
}


## Example of use ----------------

# a <- as.Date(seq(as.POSIXct('1987-05-29 018:07:00'), len = 26, by = "21 day"))
# b <- as.Date(seq(as.POSIXct('2002-05-29 018:07:00'), len = 26, by = "21 day"))
# age <- calc_age(a, units = 'years', precise = FALSE)
# age
# age <- calc_age(a, units = 'months')
# age
# age <- calc_age(a, as.Date('2005-09-01'))
# age
