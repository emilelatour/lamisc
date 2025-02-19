% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_ttest.R
\name{calc_ttest_1}
\alias{calc_ttest_1}
\title{Internal function - one-sample t-test used in public function calc_ttest()}
\usage{
calc_ttest_1(
  data,
  var,
  mu = 0,
  conf_level = 0.95,
  show_cohens_d = FALSE,
  show_alternative = FALSE,
  include_perm = FALSE,
  n_perms = 10000,
  include_np = FALSE
)
}
\arguments{
\item{data}{A data frame or tibble.}

\item{var}{A (non-empty) numeric vector of data values.}

\item{mu}{A number indicating the true value of the mean (or difference in
means if you are performing a two sample test).}

\item{conf_level}{Confidence level of the interval. Default level is 0.95.}

\item{show_cohens_d}{Logical whether to return the effect size, Cohen's d.
T-test conventional effect sizes, proposed by Cohen, are: 0.2 (small effect),
0.5 (moderate effect) and 0.8 (large effect) (Cohen 1998, Navarro (2015)).
This means that if two groups' means don’t differ by 0.2 standard
deviations or more, the difference is trivial, even if it is statistically
significant.}

\item{show_alternative}{Logical whether to show alternative hypothesis
notation in the test results. Default is FALSE.}

\item{include_perm}{Logical indicating whether to perform a permutation test
in addition to the t-test. Default is FALSE.}

\item{n_perms}{Number of permutations to perform for the permutation test.
Default is 10000.}

\item{include_np}{Logical indicating whether to perform a non-parametric test
(Wilcoxon rank-sum or Mann-Whitney U test) in addition to the t-test. Default is FALSE.}
}
\description{
Internal function - one-sample t-test used in public function calc_ttest()
}
\keyword{internal}
