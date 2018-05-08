% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_table.R
\name{make_table}
\alias{make_table}
\title{More consistent version of base::table()}
\usage{
make_table(df, x_var, y_var, x_lvls = NULL, y_lvls = NULL, labs = c(NA,
  NA), useNA = "ifany")
}
\arguments{
\item{df}{A data frame or tibble}

\item{x_var}{X variable, the one along the horizontal (top) of the table}

\item{y_var}{Y variable, the one along the vertical (side) of the table}

\item{x_lvls}{(optional) levels for the X variable}

\item{y_lvls}{(optional) levels for the Y variable}

\item{labs}{(optional) labels for the X and Y variables}

\item{useNA}{useNA controls if the table includes counts of NA values: the
allowed values correspond to never ("no"), only if the count is positive
("ifany") and even for zero counts ("always")}
}
\value{
A table
}
\description{
Takes a data frame and the columns specified and creates a table object that
is clean and easy to use for the rest of the calculations. It helps to
resolve some issues with the base R table function that doesn't handle empty
cells very well IMHO.
}
\examples{
df <- tibble::tribble(
  ~a, ~b, ~c,
  1L, 1L, 1L,
  0L, 1L, 1L,
  1L, 1L, 0L,
  0L, 1L, 0L,
  1L, 1L, 1L,
  0L, 1L, 1L,
  1L, 1L, 0L
)

make_table(df = df, x_var = a, y_var = b)
}