% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_surv_p_table.R
\name{calc_surv_p_table}
\alias{calc_surv_p_table}
\title{Calculate time-to-event estimates}
\usage{
calc_surv_p_table(
  fit,
  times = c(0, 12, 24, 36, 48),
  all_group_name = "All subjects"
)
}
\arguments{
\item{fit}{\code{survfit} object}

\item{times}{A numeric vector}

\item{all_group_name}{String name for all records. Default is "All subjects".}
}
\value{
A tibble
}
\description{
Takes a \code{survfit} object and returns a tibble with proportion surviving at
given times and confidence intervals.
}
\examples{
library(survival)
library(dplyr)
head(aml)

fit <- survival::survfit(
  survival::Surv(time = time,
                 event = status) ~ x,
  data = aml)

calc_surv_p_table(fit)
calc_surv_p_table(fit, times = c(6, 18, 30))

fit <- survival::survfit(
  survival::Surv(time = time,
                 event = status) ~ 1,
  data = aml)

calc_surv_p_table(fit)
calc_surv_p_table(fit, times = c(6, 18, 30))

}
