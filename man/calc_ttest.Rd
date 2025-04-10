% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_ttest.R
\name{calc_ttest}
\alias{calc_ttest}
\alias{calc_ttest_i}
\title{Calculate a t-test and summary statistics}
\usage{
calc_ttest(
  data,
  var,
  by = NULL,
  mu = 0,
  paired = FALSE,
  var_equal = FALSE,
  df_form = "Satterthwaite",
  conf_level = 0.95,
  check_variance = FALSE,
  reverse_groups = FALSE,
  show_cohens_d = FALSE,
  show_alternative = FALSE,
  include_perm = FALSE,
  n_perms = 10000,
  include_np = FALSE,
  fmt_res = FALSE,
  accuracy = 0.1,
  ...
)

calc_ttest_i(
  n1,
  m1,
  s1,
  n2 = NULL,
  m2 = NULL,
  s2 = NULL,
  mu = 0,
  var_equal = FALSE,
  df_form = "Satterthwaite",
  conf_level = 0.95,
  check_variance = FALSE,
  reverse_groups = FALSE,
  show_cohens_d = FALSE,
  show_alternative = FALSE,
  include_perm = FALSE,
  n_perms = 10000,
  include_np = FALSE,
  fmt_res = FALSE,
  accuracy = 0.1,
  ...
)
}
\arguments{
\item{data}{A data frame or tibble.}

\item{var}{A (non-empty) numeric vector of data values.}

\item{by}{A factor (or character) with two levels giving the corresponding groups.}

\item{mu}{A number indicating the true value of the mean (or difference in
means if you are performing a two sample test).}

\item{paired}{A logical indicating whether you want a paired t-test.}

\item{var_equal}{logical variable indicating whether to treat the two
variances as being equal. If TRUE then the pooled variance is used to
estimate the variance otherwise if FALSE then unequal variance is assumed
and the Welch (or Satterthwaite) approximation to the degrees of freedom is
used.}

\item{df_form}{If unequal variances are assumed, then specify to use "Welch"
or "Satterthwaite" (default) approximation to the degrees of freedom. R
\code{t.test} documentation says it uses "Welch" but actually they use
"Satterthwaite". At least, "Welch" in R matches "Satterthwaite" in Stata.}

\item{conf_level}{Confidence level of the interval. Default level is 0.95.}

\item{check_variance}{A logical whether to return some tests of homogeneity
of variances. Bartlett's, Levene's, Fligner, and check of the ratio of the
standard deviations.}

\item{reverse_groups}{If TRUE, then uses \code{forcats::fct_rev()} to reverse the
order of the groups being compared. Default is FALSE.}

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

\item{fmt_res}{Logical whether to format estimates and p-values for the summary_stats,
hypothesis_tests, tests_of_homogeneity_of_variance, and variance_check.}

\item{accuracy}{A number to round to. Use (e.g.) 0.01 to show 2 decimal
places of precision. If NULL, the default, uses a heuristic that should
ensure breaks have the minimum number of digits needed to show the
difference between adjacent values.}

\item{...}{Additional arguments passed to \code{scales::number()}}

\item{n1}{Sample size of group 1}

\item{m1}{Mean for group 1}

\item{s1}{Standard deviation for group 1}

\item{n2}{Sample size of group 2 (if applicable)}

\item{m2}{Mean for group 2 (if applicable)}

\item{s2}{Standard deviation for group 2 (if applicable)}
}
\value{
A list
\itemize{
\item summary_stats - Summary statistics. Estimates and confidence intervals.
\item difference - A statement of the Null hypothesis.
\item method - What test is being used. Character string.
\item hypothesis_tests - Test statistics, p-values, estimates and
confidence intervals for two-sided and one-sided tests.
\item tests_of_homogeneity_of_variance - A few difference checks for
homogeneity of variances. Levene's test and the F test are fragile w.r.t to
normality. Don't rely on these. Levene's test is an example of a test where
alpha should be set quite high, say 0.10 or 0.15. This is because Type II
error for this test are more severe (claim equal variances when they are
not). F-test: Compare the variances of two samples. The data must be
normally distributed. Bartlett’s test: Compare the variances of k samples,
where k can be more than two samples. The data must be normally
distributed. The Levene test is an alternative to the Bartlett test that is
less sensitive to departures from normality. Levene’s test: Compare the
variances of k samples, where k can be more than two samples. It’s an
alternative to the Bartlett’s test that is less sensitive to departures
from normality. Fligner-Killeen test: a non-parametric test which is very
robust against departures from normality.
\item variance_check - A variance check. If the ratio of SDmax and SDmin
is less than 2 (SDmax / SDmin < 2) the the pooled estimate of the common
variance can be used. (Use \code{var_equal = TRUE}). If the ratio of population
standard deviations is "close" to 3, then the test can suffer if the sample
sizes are dissimilar (even worse if it's the smaller sample size has larger
variance). If distributions are slightly skewed, then skewness should be in
the same direction and sample sizes should be nearly equal to minimize the
impact of this problem. For practical purposes, t-tests give good results
when data are approximately symmetric and the ratio of sample standard
deviations doesn't exceed 2 (e.g. SDmax / SDmin < 2).

}

A list
}
\description{
Function to return tidy results for a t-test along with the summary
statistics if the groups. Fashioned after the t-test output in Stata and the
details given there. Also, if requested, checks for equal variance and
Cohen's d are returned.

Calculate a t-test (immediate form) and summary statistics. Similar to
\code{calc_ttest()} except that we specify summary statistics rather than
variables and data as arguments.

Function to return tidy results for a t-test along with the summary
statistics if the groups. Fashioned after the t-test output in Stata and the
details given there. Also, if requested, checks for equal variance and
Cohen's d are returned.
}
\examples{
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)

fuel <- tibble::tribble(
  ~mpg, ~treated,
  20L,       0L,
  23L,       0L,
  21L,       0L,
  25L,       0L,
  18L,       0L,
  17L,       0L,
  18L,       0L,
  24L,       0L,
  20L,       0L,
  24L,       0L,
  23L,       0L,
  19L,       0L,
  24L,       1L,
  25L,       1L,
  21L,       1L,
  22L,       1L,
  23L,       1L,
  18L,       1L,
  17L,       1L,
  28L,       1L,
  24L,       1L,
  27L,       1L,
  21L,       1L,
  23L,       1L
)


#### One sample t-test --------------------------------

calc_ttest(data = fuel,
           var = mpg)


calc_ttest(data = fuel,
           var = mpg,
           mu = 20)

calc_ttest(data = fuel,
           var = mpg,
           mu = 20,
           show_cohens_d = TRUE)


calc_ttest(data = fuel,
           var = mpg,
           mu = 20,
           fmt_res = TRUE)

calc_ttest(data = fuel,
           var = mpg,
           mu = 0,
           include_perm = TRUE)


calc_ttest(data = fuel,
           var = mpg,
           mu = 0,
           include_perm = TRUE,
           include_np = TRUE,
           fmt_res = FALSE)

calc_ttest(data = fuel,
           var = mpg,
           mu = 0,
           include_perm = TRUE,
           include_np = TRUE,
           fmt_res = TRUE)


# Plot the permutation distribution

(res <- calc_ttest(data = fuel,
                   var = mpg,
                   mu = 0,
                   include_perm = TRUE))


perm_results_df <- res$permutation_distribution

observed <- res |>
  purrr::pluck("summary_stats",
               "mean",
               1)

# Create a histogram with ggplot2
calc_bw <- function(x) {

  x <- na.omit(x)

  # Calculate the Sturges' number of bins
  n <- length(x)  # Number of observations
  k <- ceiling(log2(n) + 1)  # Number of bins using Sturges' formula

  # Calculate the bin width
  data_range <- max(x) - min(x)  # Range of the data
  bin_width <- data_range / k

  return(bin_width)

}

ggplot(data = perm_results_df,
       aes(x = test_stat)) +
  geom_histogram(binwidth = calc_bw(perm_results_df$test_stat),
                 colour = "white",
                 alpha = 0.7) +
  geom_vline(xintercept = observed,
             colour = "#D5006A",
             linewidth = 2) +
  theme_minimal() +
  labs(
    title = "Permutation Test Statistic Distribution",
    x = "Test Statistic",
    y = "Frequency"
  )


#### Paired t-test --------------------------------

calc_ttest(data = fuel,
           var = mpg,
           by = treated,
           paired = TRUE)

calc_ttest(data = fuel,
           var = mpg,
           by = treated,
           paired = TRUE,
           include_perm = TRUE)


#### 2-sample t-test --------------------------------

calc_ttest(data = fuel,
           var = mpg,
           by = treated,
           paired = FALSE)

calc_ttest(data = fuel,
           var = mpg,
           by = treated,
           paired = FALSE,
           check_variance = TRUE)

calc_ttest(data = fuel,
           var = mpg,
           by = treated,
           paired = FALSE,
           check_variance = TRUE,
           fmt_res = TRUE)


calc_ttest(data = fuel,
           var = mpg,
           by = treated,
           df_form = "Satterthwaite",
           paired = FALSE)

# Compare to Base R
with(fuel, t.test(mpg ~ treated))


calc_ttest(data = fuel,
           var = mpg,
           by = treated,
           df_form = "Welch",
           paired = FALSE)


calc_ttest(data = fuel,
           var = mpg,
           by = treated,
           mu = 0,
           include_perm = TRUE,
           include_np = TRUE,
           fmt_res = FALSE)

calc_ttest(data = fuel,
           var = mpg,
           by = treated,
           mu = 0,
           include_perm = TRUE,
           include_np = TRUE,
           fmt_res = TRUE)


# Plot the permutation distribution

(res <- calc_ttest(data = fuel,
                   var = mpg,
                   by = treated,
                   mu = 0,
                   include_perm = TRUE))


perm_results_df <- res$permutation_distribution

observed <- res |>
  purrr::pluck("summary_stats",
               "mean",
               4)

# Create a histogram with ggplot2
calc_bw <- function(x) {

  x <- na.omit(x)

  # Calculate the Sturges' number of bins
  n <- length(x)  # Number of observations
  k <- ceiling(log2(n) + 1)  # Number of bins using Sturges' formula

  # Calculate the bin width
  data_range <- max(x) - min(x)  # Range of the data
  bin_width <- data_range / k

  return(bin_width)

}

ggplot(data = perm_results_df,
       aes(x = test_stat)) +
  geom_histogram(binwidth = calc_bw(perm_results_df$test_stat),
                 colour = "white",
                 alpha = 0.7) +
  geom_vline(xintercept = observed,
             colour = "#D5006A",
             linewidth = 2) +
  theme_minimal() +
  labs(
    title = "Permutation Test Statistic Distribution",
    x = "Test Statistic",
    y = "Frequency"
  )


# library(infer)
#
# null_dist <- fuel |>
#   mutate(treated = factor(treated)) |>
#   infer::specify(formula = mpg ~ treated) \%>\%
#   hypothesize(null = "independence") \%>\%
#   generate(reps = 1000, type = "permute") |>
#   calculate(stat = "diff in means")
#
# obs_diff_means <- fuel |>
#   mutate(treated = factor(treated)) |>
#   infer::specify(formula = mpg ~ treated) \%>\%
#   calculate(stat = "diff in means")
#
# infer::visualise(null_dist) +
#   shade_p_value(obs_stat = obs_diff_means, direction = "both")
#
# null_dist |>
#   get_p_value(obs_stat = obs_diff_means, direction = "both")
#
#
# null_dist |>
#   get_p_value(obs_stat = obs_diff_means, direction = "greater")
#### Immediate form --------------------------------

# One sample t-test
calc_ttest_i(n1 = 24, m1 = 21.88, s1 = 3.06)

# Two sample t-test (only available for unpaired)
calc_ttest_i(n1 = 12, m1 = 21.0, s1 = 2.7,
             n2 = 12, m2 = 22.75, s2 = 3.3)
}
