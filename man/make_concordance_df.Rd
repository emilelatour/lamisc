% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_concordance_df.R
\name{make_concordance_df}
\alias{make_concordance_df}
\title{Make a table of concordance for multi-rater kappa}
\usage{
make_concordance_df(df, id, ..., keep_id = TRUE)
}
\arguments{
\item{df}{A data frame or tibble.}

\item{id}{A column/variable (any type) that uniquely identifies the unit being
rated (i.e. patient id, ssn, name, etc.).}

\item{...}{The columns of ratings by the raters, character or factor.}

\item{keep_id}{Logical; if \code{TRUE} then it will keep the \code{id} column
else it will be dropped.}
}
\value{
A tibble
}
\description{
This function is a helper for working with the \code{concordance} function
from the \code{raters} package. Rather than a table or the original columns of data,
the \code{concordance} function takes, as an input, a data frame that shows
for each thing being rated (patient, sample, etc.) the number of raters that
gave a particular rating. It creates a data frame of counts of raters by
ratee and rating.
}
\examples{
library(tibble)
library(raters)
diagnostic_df <-  data.frame(stringsAsFactors=FALSE,
         id = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L,
                15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L,
                27L, 28L, 29L, 30L),
rater_1 = c("Neurosis", "Personality disorders", "Schizophrenia", "Other",
            "Personality disorders", "Depression", "Schizophrenia",
            "Depression", "Depression", "Other", "Depression", "Depression",
            "Personality disorders", "Depression", "Personality disorders",
            "Schizophrenia", "Depression", "Depression", "Personality disorders",
            "Depression", "Other", "Neurosis", "Personality disorders",
            "Depression", "Depression", "Personality disorders", "Depression",
            "Personality disorders", "Depression", "Other"),
rater_2 = c("Neurosis", "Personality disorders", "Schizophrenia", "Other",
            "Personality disorders", "Depression", "Schizophrenia",
            "Depression", "Depression", "Other", "Neurosis",
            "Personality disorders", "Personality disorders", "Neurosis",
            "Personality disorders", "Schizophrenia", "Depression", "Depression",
            "Personality disorders", "Schizophrenia", "Other", "Neurosis",
            "Personality disorders", "Depression", "Neurosis", "Personality disorders",
            "Depression", "Personality disorders", "Schizophrenia", "Other"),
rater_3 = c("Neurosis", "Personality disorders", "Schizophrenia", "Other",
            "Personality disorders", "Schizophrenia", "Schizophrenia",
            "Schizophrenia", "Neurosis", "Other", "Neurosis", "Neurosis",
            "Personality disorders", "Neurosis", "Neurosis", "Schizophrenia",
            "Depression", "Depression", "Neurosis", "Schizophrenia", "Other",
            "Neurosis", "Neurosis", "Neurosis", "Neurosis",
            "Personality disorders", "Depression", "Neurosis", "Schizophrenia", "Other"),
rater_4 = c("Neurosis", "Other", "Schizophrenia", "Other", "Neurosis",
            "Schizophrenia", "Schizophrenia", "Schizophrenia", "Neurosis",
            "Other", "Neurosis", "Neurosis", "Schizophrenia", "Neurosis",
            "Neurosis", "Schizophrenia", "Neurosis", "Depression",
            "Neurosis", "Other", "Other", "Neurosis", "Other", "Neurosis", "Neurosis",
            "Personality disorders", "Depression", "Neurosis",
            "Schizophrenia", "Other"),
rater_5 = c("Neurosis", "Other", "Personality disorders", "Other",
            "Neurosis", "Schizophrenia", "Other", "Schizophrenia",
            "Neurosis", "Other", "Neurosis", "Neurosis", "Schizophrenia",
            "Neurosis", "Neurosis", "Schizophrenia", "Other", "Depression",
            "Neurosis", "Other", "Other", "Neurosis", "Other", "Neurosis",
            "Neurosis", "Personality disorders", "Other", "Neurosis", "Schizophrenia",
            "Other"),
rater_6 = c("Neurosis", "Other", "Other", "Other", "Neurosis",
            "Schizophrenia", "Other", "Neurosis", "Neurosis", "Other",
            "Neurosis", "Neurosis", "Schizophrenia", "Neurosis", "Other",
            "Other", "Other", "Personality disorders", "Neurosis", "Other",
            "Other", "Personality disorders", "Other", "Neurosis", "Other",
            "Neurosis", "Other", "Neurosis", "Schizophrenia", "Other")
)

diagnostic_2 <- make_concordance_df(df = diagnostic_df,
                                  id = id,
                                  rater_1:rater_6,
                                  keep_id = FALSE)
diagnostic_2
data("diagnostic")
raters::concordance(diagnostic, test = "Chisq")
raters::concordance(diagnostic_2, test = "Chisq")
}
