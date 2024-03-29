% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drop-fct-lvls2.R
\name{drop_lvls2}
\alias{drop_lvls2}
\title{Drop factor levels and filter the data the same time}
\usage{
drop_lvls2(data, var, lvls_to_drop = NULL)
}
\arguments{
\item{data}{A data frame or tibble. Or a survey object (class = \code{tbl_svy},
\code{survey.design2}, or \code{survey.design})}

\item{var}{A factor (or character vector).}

\item{lvls_to_drop}{Character vector of factors to drop}
}
\value{
An object of the same type as data.
}
\description{
When you filter data to remove rows that match a level in a factor, the
factor levels aren't removed. This function will filter the data and drop the
factor levels.
}
\examples{
library(dplyr)
library(tibble)
library(palmerpenguins)
library(srvyr)
library(survey)

# With a data frame
df <- tibble::tibble(letters = letters[1:5],
                     numbers = seq(1:5))

f_lvls <- letters[1:6]

df <- df \%>\%
  mutate(letters = factor(letters,
                          levels = f_lvls))

levels(df$letters)

df \%>\%
  drop_lvls2(data = .,
            var = letters,
            lvls = "a") \%>\%
  dplyr::count(letters,
               .drop = FALSE)

df \%>\%
  drop_lvls2(data = .,
            var = letters,
            lvls = c("a", "b", "g")) \%>\%
  dplyr::count(letters,
               .drop = FALSE)


# With a tibble
penguins \%>\%
  dplyr::count(species)

penguins \%>\%
  drop_lvls2(data = .,
            var = species,
            lvls = c("Chinstrap")) \%>\%
  dplyr::count(species)

penguins \%>\%
  drop_lvls2(data = .,
            var = species,
            lvls = c("Chinstrap",
                     "Gentoo")) \%>\%
  dplyr::count(species)


# With a survey using srvyr package
data(api)

srs_design_srvyr <- apisrs \%>\% as_survey_design(ids = 1, fpc = fpc)
srs_design_survey <- svydesign(ids = ~1, fpc = ~fpc, data = apisrs)

srs_design_srvyr \%>\%
  group_by(stype) \%>\%
  summarize(proportion = survey_mean(),
            total = survey_total())

srs_design_srvyr \%>\%
  drop_lvls2(data = .,
            var = stype,
            lvls_to_drop = "H") \%>\%
  group_by(stype) \%>\%
  summarize(proportion = survey_mean(),
            total = survey_total())

srs_design_srvyr \%>\%
  drop_lvls2(data = .,
            var = stype,
            lvls_to_drop = c("H", "M")) \%>\%
  group_by(stype) \%>\%
  summarize(proportion = survey_mean(),
            total = survey_total())

# With a survey using survey package
svymean(~stype, srs_design_survey)
svytotal(~stype, srs_design_survey)

srs_design_survey2 <- srs_design_survey \%>\%
  drop_lvls2(data = .,
            var = stype,
            lvls_to_drop = c("H"))

svymean(~stype, srs_design_survey2)
svytotal(~stype, srs_design_survey2)
}
