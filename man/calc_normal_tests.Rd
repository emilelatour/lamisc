% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_normal_tests.R
\name{calc_normal_tests}
\alias{calc_normal_tests}
\title{Tests of normality}
\usage{
calc_normal_tests(data, var)
}
\arguments{
\item{data}{A tibble or data frame.}

\item{var}{A numeric vector of data values.}
}
\value{
A tibble
}
\description{
Performs the following tests of normality given a data.frame and column:
\itemize{
\item Shapiro-Francia test for the composite hypothesis of normality,
\item (Robust) Jarque-Bera test of normality,
\item Shapiro-Wilk test of normality,
\item Anderson-Darling test for the composite hypothesis of normality,
\item One--sample Kolmogorov-Smirnov test.
}

Stata recommends to use the Shapiro-Francia test whenever possible. Note that
NAs are removed by default.
\itemize{
\item Null hypothesis: the data follows a normal distribution.
\item Alternative hypothesis: the data does not follow a normal distribution.
}
}
\examples{
library(dplyr)
library(tibble)
data <- tibble::tibble(
  x = c(rnorm(99, mean = 5, sd = 3), NA),
  y = runif(100, min = 2, max = 4),
  z = rnorm(100, mean = 2, sd = 3))

calc_normal_tests(data = data,
                  var = x)

calc_normal_tests(data = data,
                  var = y)


calc_normal_tests(data = data,
                  var = z)
}
