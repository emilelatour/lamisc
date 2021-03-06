% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/requery_cormat.R
\name{rquery_cormat}
\alias{rquery_cormat}
\title{Correlation matrix}
\usage{
rquery_cormat(
  x,
  type = c("lower", "upper", "full", "flatten"),
  graph = TRUE,
  graphType = c("correlogram", "heatmap"),
  col = NULL,
  ...
)
}
\arguments{
\item{x}{matrix of data values}

\item{type}{Possible values are “lower” (default), “upper”, “full” or
“flatten”. It displays the lower or upper triangular of the matrix, full or
flatten matrix.}

\item{graph}{if TRUE, a correlogram or heatmap is generated to visualize the
correlation matrix.}

\item{graphType}{Type of graphs. Possible values are “correlogram” or
“heatmap”.}

\item{col}{colors to use for the correlogram or the heatmap.}

\item{...}{Further arguments to be passed to cor() or cor.test() function.}
}
\value{
A list containing the following: \describe{ \item{r}{table of correlation
coefficients} \item{p}{Table of p-values corresponding to the significance
levels of the correlations} \item{sym}{A representation of the correlation
matrix in which coefficients are replaced by symbols according to the
strength of the dependence} }
}
\description{
Calculate and easily visualize a correlation matrix. Function comes from \href{http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need}{Correlation matrix : An R function to do all you need}
}
\examples{
mydata <- mtcars[, c(1,3,4,5,6,7)]
head(mydata)

# Computing the correlation matrix
rquery_cormat(mydata)

# Upper triangle of the correlation matrix
rquery_cormat(mydata, type = "upper")

# Full correlation matrix
rquery_cormat(mydata, type = "full")
}
