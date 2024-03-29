% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max_pad.R
\name{max_pad}
\alias{max_pad}
\title{Pad a string with the maximum width}
\usage{
max_pad(string, width = 0, side = c("left", "right", "both"), pad = " ")
}
\arguments{
\item{string}{A character vector.}

\item{width}{Minimum width of padded strings.}

\item{side}{Side on which padding character is added (left, right or both).}

\item{pad}{Single padding character (default is a space).}
}
\value{
A character vector.
}
\description{
Vectorised over string, width and pad. Pad to the maximum width in a string.
}
\examples{
x <- c("abc", "123456", "defg")

max_pad(x) 
max_pad(x, side = "right") 
max_pad(x, pad = "0") 
max_pad(x, side = "right", pad = "0") 
max_pad(x, side = "both", pad = "0") 
}
