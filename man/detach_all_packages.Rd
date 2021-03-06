% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detach_all_packages.R
\name{detach_all_packages}
\alias{detach_all_packages}
\title{Detach all currently loaded packages}
\usage{
detach_all_packages()
}
\description{
I found this in some example code online and thought that it was super handy
seeming and wanted to save it for later.
}
\examples{
\dontrun{
# Load some packages
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
# List of loaded packages -- before
(.packages())

# Detach all packages
detach_all_packages()

# List of loaded packages -- after
(.packages())
}

}
\references{
\url{https://github.com/tomasu909/Tidy-Tuesday-Submissions/blob/master/tidytuesday2.Rmd}
}
