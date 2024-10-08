% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_fct_lvls.R
\name{get_fct_lvls}
\alias{get_fct_lvls}
\title{Get factor levels for a variable. Unique ones only or all of te}
\usage{
get_fct_lvls(data, x, unique_lvls = TRUE)
}
\arguments{
\item{data}{A data frame or tibble}

\item{x}{Column in the data}

\item{unique_lvls}{Logical. If \code{TRUE} (default), then only the possible
values of a factor are returned. If \code{FALSE}, then the original factor
levels are returned.}
}
\value{
A character vector or \code{NULL}
}
\description{
Given a data frame and a column, the function will return the factor levels.
If \code{unique_lvls = TRUE}, then it will only return the levels for which there
is a value in the data.
}
\examples{
get_fct_lvls(data = iris, x = Species)

iris2 <- subset(iris, Species != "setosa")
table(iris2$Species)
get_fct_lvls(data = iris2, x = Species)
get_fct_lvls(data = iris2, x = Species, unique_lvls = FALSE)
}
\references{
\url{https://chemicalstatistician.wordpress.com/2018/03/10/use-unique-instead-of-levels-to-find-the-possible-values-of-a-character-variable-in-r/}
}
