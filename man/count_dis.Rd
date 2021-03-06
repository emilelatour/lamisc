% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_dis.R
\name{count_dis}
\alias{count_dis}
\title{Count distinct/unique rows}
\usage{
count_dis(.data, ..., .keep_all = FALSE)
}
\arguments{
\item{.data}{a tbl}

\item{...}{Optional variables to use when determining uniqueness. If there
are multiple rows for a given combination of inputs, only the first row
will be preserved. If omitted, will use all variables.}

\item{.keep_all}{If \verb{TRUE``, keep all variables in }.data\verb{. If a combination of }...`` is not distinct, this keeps the first row of values.}
}
\value{
A numeric value
}
\description{
This is a pretty simple wrapper around \code{dplyr::distinct()}. I tend to do this
pipe chain alot when working with data with multiple row per subjects. Making
this for my own convenience more than anything else.
}
\examples{
library(dplyr)
library(tibble)

df <- tibble::tibble(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)

count_dis(df, x, y)
count_dis(df, x, y)
}
