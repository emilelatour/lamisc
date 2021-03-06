% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_list.R
\name{read_list}
\alias{read_list}
\alias{read_list2}
\title{Read a lot of datasets at once with R}
\usage{
read_list(list_of_datasets, read_func)

read_list2(list_of_datasets, read_func, envir = .GlobalEnv)
}
\arguments{
\item{list_of_datasets}{A character vector with names of data sets to read.}

\item{read_func}{Function to read the data sets.}

\item{envir}{Environment to assign data frames to. Default is \code{.GlobalEnv}.
Used in \code{read_list2} only.}
}
\value{
A list of data sets
}
\description{
Supply a list of data sets and the function to read the datasets. For
example, to read in \code{.csv} files, use \code{readr::read_csv}. This function comes
from Bruno Rodrigues' website and credit goes to him.

If you prefer not to have the datasets in a list, but rather import them into
the global environment, then use \code{read_list2}.
}
\examples{
\dontrun{
library(dplyr)
library(stringr)
library(readr)

csv_path <- "C:/Users/latour/Dropbox/repos/lamisc/inst/extdata"

(data_files <- list.files(path = csv_path,
                          pattern = ".csv"))

setwd(csv_path)

#### read_list --------------------------------
list_of_data_sets <- read_list(list_of_datasets = data_files,
                               read_func = readr::read_csv)

dplyr::glimpse(list_of_data_sets)


#### read_list2 --------------------------------
read_list2(list_of_datasets = data_files,
           read_func = readr::read_csv)
}
}
\references{
\url{http://www.brodrigues.co/blog/2016-07-26-read-a-lot-of-datasets-at-once-with-r/}
}
