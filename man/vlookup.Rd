% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vlookup.R
\name{vlookup}
\alias{vlookup}
\title{Lookup and retrieve data from a specific column in table}
\usage{
vlookup(this, data, key, value)
}
\arguments{
\item{this}{Value or vector; what you are trying to look up}

\item{data}{Data frame; the data where you want to match \code{this}}

\item{key}{The name (quoted) of the column to match on}

\item{value}{The name (quoted) of the column to return}
}
\value{
A vector of values
}
\description{
If you've ever used excel, then you are hopefully familiar with the useful
\code{vlookup}. This is a fun little version of it in R that was tweeted by Jenny
Bryan at some point.

Match on the \code{key} and return \code{value}.
}
\examples{
library(dplyr)
head(starwars)

c("Luke Skywalker", "Jabba Desilijic Tiure", "Yoda") \%>\%
  vlookup(this = .,
          data = starwars,
          key = "name",
          value = "species")

c("Luke Skywalker", "Jabba Desilijic Tiure", "Yoda") \%>\%
  vlookup(this = ., data = starwars, key = "name", value = "mass") \%>\%
  sum(.)

c("Luke Skywalker", "Jabba Desilijic Tiure", "Yoda") \%>\%
  vlookup(data = starwars, key = "name", value = "mass") \%>\%
  sum(.)
}
\references{
\url{https://twitter.com/JennyBryan/status/980978609794895872/photo/1}
}
