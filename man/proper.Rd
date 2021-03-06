% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proper.R
\name{proper}
\alias{proper}
\title{Capitalize the first letter (option: in each word)}
\usage{
proper(x, just_first_word = TRUE)
}
\arguments{
\item{x}{Character; the text that should be converted to proper case.}

\item{just_first_word}{Logical; If TRUE (default) only the first letter of
the first word of the text will be capitalized, else if FALSE then the
first letter of each word will be capitalized.}
}
\value{
A chacter string
}
\description{
This is a riff on the Excel PROPER function which capitalizes the words in a
given text string. Numbers and punctuation are not affected.
}
\examples{
sample <- c("zachary taylor", "To be or not to be", "san diego, CA")
proper(sample)
proper(sample, just_first_word = FALSE)

another_sample <- c("this is a TITLE", "2-way street", "76BudGet")
proper(another_sample)
proper(another_sample, just_first_word = FALSE)

}
