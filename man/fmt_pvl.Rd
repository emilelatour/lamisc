% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatters.R
\name{fmt_pvl}
\alias{fmt_pvl}
\title{p-values formatter}
\usage{
fmt_pvl(
  x,
  accuracy = 0.001,
  decimal.mark = ".",
  add_p = FALSE,
  as_factor = FALSE
)
}
\arguments{
\item{x}{A numeric vector of p-values.}

\item{accuracy}{Number to round to.}

\item{decimal.mark}{The character to be used to indicate the numeric
decimal point.}

\item{add_p}{Add "P =" before the value?}

\item{as_factor}{Logical; if TRUE, a factor is returned}
}
\value{
\code{pvalue_format} returns a function with single parameter
\code{x}, a numeric vector, that returns a character vector.
}
\description{
Formatter for p-values, adding a symbol "<" for small p-values. Adapted
directly from \code{scales} package.
}
\examples{
p <- c(.50, 0.12, .045, .011, .009, .00002, NA)
fmt_pvl(p)
fmt_pvl(p, accuracy = .01)
fmt_pvl(p, add_p = TRUE)
}
