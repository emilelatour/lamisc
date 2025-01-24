

#' @title
#' Calculate a t-test and summary statistics
#'
#' @description
#' Function to return tidy results for a t-test along with the summary
#' statistics if the groups. Fashioned after the t-test output in Stata and the
#' details given there. Also, if requested, checks for equal variance and
#' Cohen's d are returned.
#'
#'
#' @param data A data frame or tibble.
#' @param var A (non-empty) numeric vector of data values.
#' @param by A factor (or character) with two levels giving the corresponding groups.
#' @param mu A number indicating the true value of the mean (or difference in
#'   means if you are performing a two sample test).
#' @param paired A logical indicating whether you want a paired t-test.
#' @param var_equal  logical variable indicating whether to treat the two
#'   variances as being equal. If TRUE then the pooled variance is used to
#'   estimate the variance otherwise if FALSE then unequal variance is assumed
#'   and the Welch (or Satterthwaite) approximation to the degrees of freedom is
#'   used.
#' @param df_form If unequal variances are assumed, then specify to use "Welch"
#'   or "Satterthwaite" (default) approximation to the degrees of freedom. R
#'   `t.test` documentation says it uses "Welch" but actually they use
#'   "Satterthwaite". At least, "Welch" in R matches "Satterthwaite" in Stata.
#' @param conf_level  Confidence level of the interval. Default level is 0.95.
#' @param check_variance A logical whether to return some tests of homogeneity
#'   of variances. Bartlett's, Levene's, Fligner, and check of the ratio of the
#'   standard deviations.
#' @param reverse_groups If TRUE, then uses `forcats::fct_rev()` to reverse the
#'   order of the groups being compared. Default is FALSE.
#' @param show_cohens_d Logical whether to return the effect size, Cohen's d.
#'   T-test conventional effect sizes, proposed by Cohen, are: 0.2 (small effect),
#'   0.5 (moderate effect) and 0.8 (large effect) (Cohen 1998, Navarro (2015)).
#'   This means that if two groups' means don’t differ by 0.2 standard
#'   deviations or more, the difference is trivial, even if it is statistically
#'   significant.
#' @param show_alternative Logical whether to show alternative hypothesis
#'   notation in the test results. Default is FALSE.
#' @param include_perm Logical indicating whether to perform a permutation test
#'   in addition to the t-test. Default is FALSE.
#' @param n_perms Number of permutations to perform for the permutation test.
#'   Default is 10000.
#' @param include_np Logical indicating whether to perform a non-parametric test
#'   (Wilcoxon rank-sum or Mann-Whitney U test) in addition to the t-test. Default is FALSE.
#' @param fmt_res Logical whether to format estimates and p-values for the summary_stats,
#'   hypothesis_tests, tests_of_homogeneity_of_variance, and variance_check.
#' @param accuracy A number to round to. Use (e.g.) 0.01 to show 2 decimal
#'   places of precision. If NULL, the default, uses a heuristic that should
#'   ensure breaks have the minimum number of digits needed to show the
#'   difference between adjacent values.
#' @param ... Additional arguments passed to `scales::number()`
#'
#' @importFrom broom tidy
#' @importFrom car leveneTest
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom forcats fct_rev
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom purrr map_df
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom stats pt
#' @importFrom stats qt
#' @importFrom tibble tibble
#'
#' @return A list
#' \itemize{
#'   \item summary_stats - Summary statistics. Estimates and confidence intervals.
#'   \item difference - A statement of the Null hypothesis.
#'   \item method - What test is being used. Character string.
#'   \item hypothesis_tests - Test statistics, p-values, estimates and
#'   confidence intervals for two-sided and one-sided tests.
#'   \item tests_of_homogeneity_of_variance - A few difference checks for
#'   homogeneity of variances. Levene's test and the F test are fragile w.r.t to
#'   normality. Don't rely on these. Levene's test is an example of a test where
#'   alpha should be set quite high, say 0.10 or 0.15. This is because Type II
#'   error for this test are more severe (claim equal variances when they are
#'   not). F-test: Compare the variances of two samples. The data must be
#'   normally distributed. Bartlett’s test: Compare the variances of k samples,
#'   where k can be more than two samples. The data must be normally
#'   distributed. The Levene test is an alternative to the Bartlett test that is
#'   less sensitive to departures from normality. Levene’s test: Compare the
#'   variances of k samples, where k can be more than two samples. It’s an
#'   alternative to the Bartlett’s test that is less sensitive to departures
#'   from normality. Fligner-Killeen test: a non-parametric test which is very
#'   robust against departures from normality.
#'   \item variance_check - A variance check. If the ratio of SDmax and SDmin
#'   is less than 2 (SDmax / SDmin < 2) the the pooled estimate of the common
#'   variance can be used. (Use `var_equal = TRUE`). If the ratio of population
#'   standard deviations is "close" to 3, then the test can suffer if the sample
#'   sizes are dissimilar (even worse if it's the smaller sample size has larger
#'   variance). If distributions are slightly skewed, then skewness should be in
#'   the same direction and sample sizes should be nearly equal to minimize the
#'   impact of this problem. For practical purposes, t-tests give good results
#'   when data are approximately symmetric and the ratio of sample standard
#'   deviations doesn't exceed 2 (e.g. SDmax / SDmin < 2).
#'
#' }
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' library(tidyr)
#' library(ggplot2)
#'
#' fuel <- tibble::tribble(
#'   ~mpg, ~treated,
#'   20L,       0L,
#'   23L,       0L,
#'   21L,       0L,
#'   25L,       0L,
#'   18L,       0L,
#'   17L,       0L,
#'   18L,       0L,
#'   24L,       0L,
#'   20L,       0L,
#'   24L,       0L,
#'   23L,       0L,
#'   19L,       0L,
#'   24L,       1L,
#'   25L,       1L,
#'   21L,       1L,
#'   22L,       1L,
#'   23L,       1L,
#'   18L,       1L,
#'   17L,       1L,
#'   28L,       1L,
#'   24L,       1L,
#'   27L,       1L,
#'   21L,       1L,
#'   23L,       1L
#' )
#'
#'
#' #### One sample t-test --------------------------------
#'
#' calc_ttest(data = fuel,
#'            var = mpg)
#'
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            mu = 20)
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            mu = 20,
#'            show_cohens_d = TRUE)
#'
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            mu = 20,
#'            fmt_res = TRUE)
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            mu = 0,
#'            include_perm = TRUE)
#'
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            mu = 0,
#'            include_perm = TRUE,
#'            include_np = TRUE,
#'            fmt_res = FALSE)
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            mu = 0,
#'            include_perm = TRUE,
#'            include_np = TRUE,
#'            fmt_res = TRUE)
#'
#'
#' # Plot the permutation distribution
#'
#' (res <- calc_ttest(data = fuel,
#'                    var = mpg,
#'                    mu = 0,
#'                    include_perm = TRUE))
#'
#'
#' perm_results_df <- res$permutation_distribution
#'
#' observed <- res |>
#'   purrr::pluck("summary_stats",
#'                "mean",
#'                1)
#'
#' # Create a histogram with ggplot2
#' calc_bw <- function(x) {
#'
#'   x <- na.omit(x)
#'
#'   # Calculate the Sturges' number of bins
#'   n <- length(x)  # Number of observations
#'   k <- ceiling(log2(n) + 1)  # Number of bins using Sturges' formula
#'
#'   # Calculate the bin width
#'   data_range <- max(x) - min(x)  # Range of the data
#'   bin_width <- data_range / k
#'
#'   return(bin_width)
#'
#' }
#'
#' ggplot(data = perm_results_df,
#'        aes(x = test_stat)) +
#'   geom_histogram(binwidth = calc_bw(perm_results_df$test_stat),
#'                  colour = "white",
#'                  alpha = 0.7) +
#'   geom_vline(xintercept = observed,
#'              colour = "#D5006A",
#'              linewidth = 2) +
#'   theme_minimal() +
#'   labs(
#'     title = "Permutation Test Statistic Distribution",
#'     x = "Test Statistic",
#'     y = "Frequency"
#'   )
#'
#'
#' #### Paired t-test --------------------------------
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            paired = TRUE)
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            paired = TRUE,
#'            include_perm = TRUE)
#'
#'
#' #### 2-sample t-test --------------------------------
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            paired = FALSE)
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            paired = FALSE,
#'            check_variance = TRUE)
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            paired = FALSE,
#'            check_variance = TRUE,
#'            fmt_res = TRUE)
#'
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            df_form = "Satterthwaite",
#'            paired = FALSE)
#'
#' # Compare to Base R
#' with(fuel, t.test(mpg ~ treated))
#'
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            df_form = "Welch",
#'            paired = FALSE)
#'
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            mu = 0,
#'            include_perm = TRUE,
#'            include_np = TRUE,
#'            fmt_res = FALSE)
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            mu = 0,
#'            include_perm = TRUE,
#'            include_np = TRUE,
#'            fmt_res = TRUE)
#'
#'
#' # Plot the permutation distribution
#'
#' (res <- calc_ttest(data = fuel,
#'                    var = mpg,
#'                    by = treated,
#'                    mu = 0,
#'                    include_perm = TRUE))
#'
#'
#' perm_results_df <- res$permutation_distribution
#'
#' observed <- res |>
#'   purrr::pluck("summary_stats",
#'                "mean",
#'                4)
#'
#' # Create a histogram with ggplot2
#' calc_bw <- function(x) {
#'
#'   x <- na.omit(x)
#'
#'   # Calculate the Sturges' number of bins
#'   n <- length(x)  # Number of observations
#'   k <- ceiling(log2(n) + 1)  # Number of bins using Sturges' formula
#'
#'   # Calculate the bin width
#'   data_range <- max(x) - min(x)  # Range of the data
#'   bin_width <- data_range / k
#'
#'   return(bin_width)
#'
#' }
#'
#' ggplot(data = perm_results_df,
#'        aes(x = test_stat)) +
#'   geom_histogram(binwidth = calc_bw(perm_results_df$test_stat),
#'                  colour = "white",
#'                  alpha = 0.7) +
#'   geom_vline(xintercept = observed,
#'              colour = "#D5006A",
#'              linewidth = 2) +
#'   theme_minimal() +
#'   labs(
#'     title = "Permutation Test Statistic Distribution",
#'     x = "Test Statistic",
#'     y = "Frequency"
#'   )
#'
#'
#' # library(infer)
#' #
#' # null_dist <- fuel |>
#' #   mutate(treated = factor(treated)) |>
#' #   infer::specify(formula = mpg ~ treated) %>%
#' #   hypothesize(null = "independence") %>%
#' #   generate(reps = 1000, type = "permute") |>
#' #   calculate(stat = "diff in means")
#' #
#' # obs_diff_means <- fuel |>
#' #   mutate(treated = factor(treated)) |>
#' #   infer::specify(formula = mpg ~ treated) %>%
#' #   calculate(stat = "diff in means")
#' #
#' # infer::visualise(null_dist) +
#' #   shade_p_value(obs_stat = obs_diff_means, direction = "both")
#' #
#' # null_dist |>
#' #   get_p_value(obs_stat = obs_diff_means, direction = "both")
#' #
#' #
#' # null_dist |>
#' #   get_p_value(obs_stat = obs_diff_means, direction = "greater")
calc_ttest <- function(data,
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
                       accuracy = 0.1, ...) {

  # Fix no visible binding for global variable
  statistic <- NULL
  df <- NULL
  estimate <- NULL
  p_value <- NULL
  x <- NULL
  ratio <- NULL



  group_var <- rlang::enquo(by)
  var <- rlang::enquo(var)

  #### T-test --------------------------------

  # Perform one-sample t-test if by is NULL.
  # Perform two-sample t-test otherwise
  if (rlang::quo_is_null(group_var)) {

    var <- rlang::enquo(var)

    result <- calc_ttest_1(data = data,
                           var = !! var,
                           mu = mu,
                           conf_level = conf_level,
                           show_cohens_d = show_cohens_d,
                           show_alternative = show_alternative,
                           include_perm = include_perm,
                           n_perms = n_perms,
                           include_np = include_np)

  } else {

    result <- calc_ttest_2(data = data,
                           var = !! var,
                           by = !! group_var,
                           mu = mu,
                           paired = paired,
                           var_equal = var_equal,
                           df_form = df_form,
                           conf_level = conf_level,
                           check_variance = check_variance,
                           reverse_groups = reverse_groups,
                           show_cohens_d = show_cohens_d,
                           show_alternative = show_alternative,
                           include_perm = include_perm,
                           n_perms = n_perms,
                           include_np = include_np)



  }


  #### Format results if applicable --------------------------------

  if (fmt_res) {

    result$summary_stats <- result$summary_stats %>%
      mutate(dplyr::across(.cols = c(n, complete, missing),
                           .fns = ~ scales::number(x = .,
                                                   accuracy = 1.0)),
             dplyr::across(.cols = c(mean, sd, se, lower_ci, upper_ci),
                           .fns = ~ scales::number(x = .,
                                                   accuracy = accuracy, ...)))

    result$hypothesis_tests <- result$hypothesis_tests %>%
      mutate(dplyr::across(.cols = c(statistic, df, estimate, lower_ci, upper_ci),
                           .fns = ~ scales::number(x = .,
                                                   accuracy = accuracy, ...)),
             dplyr::across(.cols = dplyr::starts_with("p_value"),
                           .fns = ~ scales::pvalue(x = .,
                                                   accuracy = 0.001,
                                                   decimal.mark = ".",
                                                   prefix = c("< ", "", "> "),
                                                   add_p = FALSE)))

  }


  if (fmt_res & check_variance) {

    result$tests_of_homogeneity_of_variance <- result$tests_of_homogeneity_of_variance %>%
      mutate(statistic = scales::number(x = statistic,
                                        accuracy = accuracy, ...),
             dplyr::across(.cols = dplyr::starts_with("p_value"),
                           .fns = ~ scales::pvalue(x = .,
                                                   accuracy = 0.001,
                                                   decimal.mark = ".",
                                                   prefix = c("< ", "", "> "),
                                                   add_p = FALSE)))


    result$variance_check <- result$variance_check %>%
      mutate(dplyr::across(.cols = c(x, n),
                           .fns = ~ scales::number(x = .,
                                                   accuracy = 1.0)),
             dplyr::across(.cols = c(skewness, sd, ratio),
                           .fns = ~ scales::number(x = .,
                                                   accuracy = accuracy, ...)))

  }




  return(result)

}



#' @rdname calc_ttest
#'
#' @description
#' Calculate a t-test (immediate form) and summary statistics. Similar to
#' `calc_ttest()` except that we specify summary statistics rather than
#' variables and data as arguments.
#'
#' Function to return tidy results for a t-test along with the summary
#' statistics if the groups. Fashioned after the t-test output in Stata and the
#' details given there. Also, if requested, checks for equal variance and
#' Cohen's d are returned.
#'
#'
#' @param n1 Sample size of group 1
#' @param m1 Mean for group 1
#' @param s1 Standard deviation for group 1
#' @param n2 Sample size of group 2 (if applicable)
#' @param m2 Mean for group 2 (if applicable)
#' @param s2 Standard deviation for group 2 (if applicable)
#' @param var_equal  logical variable indicating whether to treat the two
#'   variances as being equal. If TRUE then the pooled variance is used to
#'   estimate the variance otherwise if FALSE then unequal variance is assumed
#'   and the Welch (or Satterthwaite) approximation to the degrees of freedom is
#'   used.
#' @param df_form If unequal variances are assumed, then specify to use "Welch"
#'   or "Satterthwaite" (default) approximation to the degrees of freedom. R
#'   `t.test` documentation says it uses "Welch" but actually they use
#'   "Satterthwaite". At least, "Welch" in R matches "Satterthwaite" in Stata.
#' @param conf_level  Confidence level of the interval. Default level is 0.95.
#' @param check_variance A logical whether to return some tests of homogeneity
#'   of variances. Bartlett's, Levene's, Fligner, and check of the ratio of the
#'   standard deviations.
#' @param reverse_groups If TRUE, then uses `forcats::fct_rev()` to reverse the
#'   order of the groups being compared. Default is FALSE.
#' @param show_cohens_d Logical whether to return the effect size, Cohen's d.
#'   T-test conventional effect sizes, proposed by Cohen, are: 0.2 (small effect),
#'   0.5 (moderate effect) and 0.8 (large effect) (Cohen 1998, Navarro (2015)).
#'   This means that if two groups' means don’t differ by 0.2 standard
#'   deviations or more, the difference is trivial, even if it is statistically
#'   significant.
#' @param show_alternative Logical whether to show alternative hypothesis
#'   notation in the test results. Default is FALSE.
#' @param include_perm Logical indicating whether to perform a permutation test
#'   in addition to the t-test. Default is FALSE.
#' @param n_perms Number of permutations to perform for the permutation test.
#'   Default is 10000.
#' @param include_np Logical indicating whether to perform a non-parametric test
#'   (Wilcoxon rank-sum or Mann-Whitney U test) in addition to the t-test. Default is FALSE.
#' @param fmt_res Logical whether to format estimates and p-values for the summary_stats,
#'   hypothesis_tests, tests_of_homogeneity_of_variance, and variance_check.
#' @param accuracy A number to round to. Use (e.g.) 0.01 to show 2 decimal
#'   places of precision. If NULL, the default, uses a heuristic that should
#'   ensure breaks have the minimum number of digits needed to show the
#'   difference between adjacent values.
#' @param ... Additional arguments passed to `scales::number()`
#'
#' @importFrom broom tidy
#' @importFrom car leveneTest
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom forcats fct_rev
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom purrr map_df
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom tibble tibble
#'
#' @return
#' A list
#'
#' @export
#'
#' @examples
#' #### Immediate form --------------------------------
#'
#' # One sample t-test
#' calc_ttest_i(n1 = 24, m1 = 21.88, s1 = 3.06)
#'
#' # Two sample t-test (only available for unpaired)
#' calc_ttest_i(n1 = 12, m1 = 21.0, s1 = 2.7,
#'              n2 = 12, m2 = 22.75, s2 = 3.3)


calc_ttest_i <- function(n1, m1, s1,
                         n2 = NULL, m2 = NULL, s2 = NULL,
                         mu = 0,
                         # paired = TRUE,
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
                         accuracy = 0.1, ...) {

  # There is no immediate form of ttest with paired data because the test is also a function of the
  # covariance, a number unlikely to be reported in any published source. For nonpaired data, however,
  # we might type

  # Fix no visible binding for global variable
  var1 <- NULL
  var <- NULL
  statistic <- NULL
  df <- NULL
  estimate <- NULL
  p_value <- NULL



  if (is.null(n2) | is.null(m2) | is.null(s2)) {

    data1 <- tibble::tibble(var1 = scale(1:n1) * s1 + m1)

    result <- calc_ttest_1(data = data1,
                           var = var1,
                           mu = mu,
                           conf_level = conf_level,
                           show_cohens_d = show_cohens_d,
                           show_alternative = show_alternative)

  } else {

    data1 <- tibble::tibble(by = "grp1",
                            var = as.numeric(scale(1:n1) * s1 + m1))

    data2 <- tibble::tibble(by = "grp2",
                            var = as.numeric(scale(1:n2) * s2 + m2))

    data12 <- dplyr::bind_rows(data1,
                               data2)

    result <- calc_ttest_2(data = data12,
                           var = var,
                           by = by,
                           mu = mu,
                           paired = FALSE,
                           var_equal = var_equal,
                           df_form = df_form,
                           conf_level = conf_level,
                           check_variance = check_variance,
                           reverse_groups = reverse_groups,
                           show_cohens_d = show_cohens_d,
                           show_alternative = show_alternative)

  }


  #### Format results if applicable --------------------------------

  if (fmt_res) {

    result$summary_stats <- result$summary_stats %>%
      mutate(dplyr::across(.cols = c(n, complete, missing),
                           .fns = ~ scales::number(x = .,
                                                   accuracy = 1.0)),
             dplyr::across(.cols = c(mean, sd, se, lower_ci, upper_ci),
                           .fns = ~ scales::number(x = .,
                                                   accuracy = accuracy, ...)))

    result$hypothesis_tests <- result$hypothesis_tests %>%
      mutate(dplyr::across(.cols = c(statistic, df, estimate, lower_ci, upper_ci),
                           .fns = ~ scales::number(x = .,
                                                   accuracy = accuracy, ...)),
             p_value = scales::pvalue(x = p_value,
                                      accuracy = 0.001,
                                      decimal.mark = ".",
                                      prefix = c("< ", "", "> "),
                                      add_p = FALSE))


  }


  return(result)

}





#' Internal function - one-sample t-test used in public function calc_ttest()
#'
#' @param data A data frame or tibble.
#' @param var A (non-empty) numeric vector of data values.
#' @param mu A number indicating the true value of the mean (or difference in
#'   means if you are performing a two sample test).
#' @param conf_level  Confidence level of the interval. Default level is 0.95.
#' @param show_cohens_d Logical whether to return the effect size, Cohen's d.
#'   T-test conventional effect sizes, proposed by Cohen, are: 0.2 (small effect),
#'   0.5 (moderate effect) and 0.8 (large effect) (Cohen 1998, Navarro (2015)).
#'   This means that if two groups' means don’t differ by 0.2 standard
#'   deviations or more, the difference is trivial, even if it is statistically
#'   significant.
#' @param show_alternative Logical whether to show alternative hypothesis
#'   notation in the test results. Default is FALSE.
#' @param include_perm Logical indicating whether to perform a permutation test
#'   in addition to the t-test. Default is FALSE.
#' @param n_perms Number of permutations to perform for the permutation test.
#'   Default is 10000.
#' @param include_np Logical indicating whether to perform a non-parametric test
#'   (Wilcoxon rank-sum or Mann-Whitney U test) in addition to the t-test. Default is FALSE.
#'
#' @importFrom broom tidy
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom purrr map_df
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#'
#' @keywords internal


calc_ttest_1 <- function(data,
                         var,
                         mu = 0,
                         conf_level = 0.95,
                         show_cohens_d = FALSE,
                         show_alternative = FALSE,
                         include_perm = FALSE,
                         n_perms = 10000,
                         include_np = FALSE) {

  # Fix no visible binding for global variable
  y <- NULL
  qt <- NULL
  lower_qt <- NULL
  upper_qt <- NULL
  alternative <- NULL
  statistic <- NULL
  parameter <- NULL
  estimate <- NULL
  conf_low <- NULL
  conf_high <- NULL
  p_value <- NULL
  alt_hypothesis <- NULL
  prob <- NULL



  var <- rlang::enquo(var)

  data <- data %>%
    dplyr::rename(y = !! var) %>%
    dplyr::select(y)

  #### Summary stats --------------------------------

  summary_stats <- data %>%
    summarise(n = length(y),
              complete = sum(!is.na(y)),
              missing = sum(is.na(y)),
              mean = mean(y, na.rm = TRUE),
              sd = sd(y, na.rm = TRUE)) %>%
    mutate(se = sd / sqrt(complete),
           lower_qt = qt(p = (1 - conf_level) / 2,
                         df = complete - 1,
                         lower.tail = TRUE),
           upper_qt = qt(p = (1 - conf_level) / 2,
                         df = complete - 1,
                         lower.tail = FALSE),
           lower_ci = mean + lower_qt * se,
           upper_ci = mean + upper_qt * se) %>%
    dplyr::select(-lower_qt,
                  -upper_qt)


  #### Hypothesis test --------------------------------

  grp1 <- data %>%
    dplyr::pull(y)

  hypothesis_tests <- c("two.sided", "less", "greater") %>%
    purrr::map_df(.x = .,
                  .f = ~ t.test(x = grp1,
                                alternative = .x,
                                mu = mu,
                                conf.level = conf_level) %>%
                    broom::tidy() %>%
                    janitor::clean_names()) %>%
    dplyr::select(alternative,
                  statistic,
                  df = parameter,
                  estimate,
                  lower_ci = conf_low,
                  upper_ci = conf_high,
                  p_value)

  method <- "One-sample t-test"


  #### Hypothesis text for output --------------------------------

  ## Null ----------------

  difference <- glue::glue("mean = mean({rlang::quo_name(var)})\nHo: mean = {mu}")

  ## Alternative ----------------

  if (show_alternative == TRUE) {

    hypothesis_tests <- hypothesis_tests %>%
      mutate(alt_hypothesis = dplyr::case_when(
        alternative == 'two.sided' ~ glue::glue('Ha: diff != {mu}'),
        alternative == 'less' ~ glue::glue('Ha: diff < {mu}'),
        alternative == 'greater' ~ glue::glue('Ha: diff > {mu}')),
        prob = c("Pr(|T| > |t|) = ",
                 "Pr(T < t) = ",
                 "Pr(T > t) = ")) %>%
      dplyr::select(alternative:upper_ci,
                    alt_hypothesis,
                    prob,
                    p_value)
  }


  #### Results in a list --------------------------------

  result_list <- list(summary_stats = summary_stats,
                      difference = difference,
                      method = method,
                      hypothesis_tests = hypothesis_tests)

  #### Permutation Test --------------------------------

  if (include_perm) {

    # Center the data around the null hypothesis
    centered_data <- grp1 - mu

    # Calculate observed test statistic
    observed <- mean(centered_data, na.rm = TRUE)

    # Generate permutation distribution by flipping signs
    perm_results <- numeric(n_perms)

    for (i in seq_len(n_perms)) {
      flipped_sample <- centered_data * sample(c(-1, 1), length(centered_data), replace = TRUE)
      perm_results[i] <- mean(flipped_sample)
    }

    # Calculate p-values
    two_sided <- mean(abs(perm_results) >= abs(observed))
    greater <- mean(perm_results >= observed)
    less <- mean(perm_results < observed)

    perm_test <- tibble::tibble(
      alternative = c("two.sided", "less", "greater"),
      p_value_perm_test = c(two_sided, less, greater)
    )

    result_list[["hypothesis_tests"]] <- result_list[["hypothesis_tests"]] %>%
      dplyr::left_join(perm_test, by = "alternative")

    result_list[["observed_difference"]] <- observed

    # Convert perm_results into a data frame
    perm_results <- tibble::tibble(test_stat = perm_results)

    result_list[["permutation_distribution"]] <- perm_results

    # # Using exactRankTests::perm.test
    # exactRankTests::perm.test(x = gss$age, mu = 40)
    # exactRankTests::perm.test(x = gss$age, mu = 40, alternative = "greater")
    # exactRankTests::perm.test(x = gss$age, mu = 40, alternative = "less")
    #
    # library(infer)
    #
    # gss %>%
    #   specify(response = age) %>%
    #   hypothesize(null = "point",
    #               mu = 0)
    #
    # gss %>%
    #   specify(response = age) %>%
    #   hypothesize(null = "point",
    #               mu = 40) |>
    #   generate(reps = 10000, type = "bootstrap") |>
    #   calculate(stat = "mean") |>
    #   mutate(observed = mean(gss$age)) |>
    #   summarise(two_sided = mean(abs(stat) >= abs(observed)),
    #             greater = mean(stat >= observed),
    #             less = mean(stat < observed))
    #
    #
    # gss_infer <- gss |>
    #   specify(response = age) |>
    #   hypothesize(null = "point",
    #               mu = 40) |>
    #   generate(reps = 1000, type = "bootstrap") |>
    #   calculate(stat = "mean")
    #
    # gss_infer
    #
    # gss_infer |> visualize() +
    #   shade_p_value(obs_stat = mean(gss$age), direction = "two_sided")


  }

  #### Non-Parametric Test --------------------------------
  if (include_np) {

    np_res <- tibble::tibble(
      alternative = c("two.sided", "less", "greater")) %>%
      mutate(p_value_non_param = purrr::map_dbl(.x = alternative,
                                                .f = ~ wilcox.test(grp1,
                                                                   mu = mu,
                                                                   alternative = .x)$p.value))

    result_list[["hypothesis_tests"]] <- result_list[["hypothesis_tests"]] %>%
      dplyr::left_join(np_res, by = "alternative")
  }


  #### Additional results --------------------------------

  ## Calculate cohen's d ----------------

  if (show_cohens_d == TRUE) {

    cohens_d <- (summary_stats[1, "mean"][[1]] - mu) / summary_stats[1, "sd"][[1]]

    result_list[["cohens_d"]] <- cohens_d

  }


  #### Return a list --------------------------------

  return(result_list)

}



#' Internal function - two-sample t-test used in public function calc_ttest()
#'
#' @param data A data frame or tibble.
#' @param var A (non-empty) numeric vector of data values.
#' @param by A factor (or character) with two levels giving the corresponding groups.
#' @param mu A number indicating the true value of the mean (or difference in
#'   means if you are performing a two sample test).
#' @param paired A logical indicating whether you want a paired t-test.
#' @param var_equal  logical variable indicating whether to treat the two
#'   variances as being equal. If TRUE then the pooled variance is used to
#'   estimate the variance otherwise if FALSE then unequal variance is assumed
#'   and the Welch (or Satterthwaite) approximation to the degrees of freedom is
#'   used.
#' @param df_form If unequal variances are assumed, then specify to use "Welch"
#'   or "Satterthwaite" (default) approximation to the degrees of freedom. R
#'   `t.test` documentation says it uses "Welch" but actually they use
#'   "Satterthwaite". At least, "Welch" in R matches "Satterthwaite" in Stata.
#' @param conf_level  Confidence level of the interval. Default level is 0.95.
#' @param check_variance A logical whether to return some tests of homogeneity
#'   of variances. Bartlett's, Levene's, Fligner, and check of the ratio of the
#'   standard deviations.
#' @param reverse_groups If TRUE, then uses `forcats::fct_rev()` to reverse the
#'   order of the groups being compared. Default is FALSE.
#' @param show_cohens_d Logical whether to return the effect size, Cohen's d.
#'   T-test conventional effect sizes, proposed by Cohen, are: 0.2 (small effect),
#'   0.5 (moderate effect) and 0.8 (large effect) (Cohen 1998, Navarro (2015)).
#'   This means that if two groups' means don’t differ by 0.2 standard
#'   deviations or more, the difference is trivial, even if it is statistically
#'   significant.
#' @param show_alternative Logical whether to show alternative hypothesis
#'   notation in the test results. Default is FALSE.
#' @param include_perm Logical indicating whether to perform a permutation test
#'   in addition to the t-test. Default is FALSE.
#' @param n_perms Number of permutations to perform for the permutation test.
#'   Default is 10000.
#' @param include_np Logical indicating whether to perform a non-parametric test
#'   (Wilcoxon rank-sum or Mann-Whitney U test) in addition to the t-test. Default is FALSE.
#'
#' @importFrom broom tidy
#' @importFrom car leveneTest
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom forcats fct_rev
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom purrr map_df
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom tibble tibble
#'
#' @keywords internal
calc_ttest_2 <- function(data,
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
                         include_np = FALSE) {

  # Fix no visible binding for global variable
  x <- NULL
  y <- NULL
  qt <- NULL
  lower_qt <- NULL
  upper_qt <- NULL
  estimate <- NULL
  pt <- NULL
  alternative <- NULL
  statistic <- NULL
  parameter <- NULL
  conf_low <- NULL
  conf_high <- NULL
  p_value <- NULL
  alt_hypothesis <- NULL
  prob <- NULL
  diff_means <- NULL



  group_var <- rlang::enquo(by)
  var <- rlang::enquo(var)


  if (reverse_groups) {

    data <- data %>%
      mutate(!! rlang::quo_name(group_var) := as.factor(!! group_var),
             !! rlang::quo_name(group_var) := forcats::fct_rev(!! group_var))

  }

  data <- data %>%
    dplyr::rename(x = !! group_var,
                  y = !! var) %>%
    dplyr::select(x, y)


  #### Summary stats by group --------------------------------

  by_group_df <- data %>%
    group_by(x) %>%
    summarise(n = length(y),
              complete = sum(!is.na(y)),
              missing = sum(is.na(y)),
              mean = mean(y, na.rm = TRUE),
              sd = sd(y, na.rm = TRUE),
              .groups = "keep") %>%
    mutate(se = sd / sqrt(complete),
           lower_qt = qt(p = (1 - conf_level) / 2,
                         df = complete - 1,
                         lower.tail = TRUE),
           upper_qt = qt(p = (1 - conf_level) / 2,
                         df = complete - 1,
                         lower.tail = FALSE),
           lower_ci = mean + lower_qt * se,
           upper_ci = mean + upper_qt * se) %>%
    dplyr::rename(group = 1) %>%
    dplyr::select(-lower_qt,
                  -upper_qt) %>%
    mutate(group = as.character(group)) %>%
    dplyr::ungroup()

  #### Summary stats for all combined --------------------------------

  combined <- data %>%
    summarise(group = "combined",
              n = length(y),
              complete = sum(!is.na(y)),
              missing = sum(is.na(y)),
              mean = mean(y, na.rm = TRUE),
              sd = sd(y, na.rm = TRUE)) %>%
    mutate(se = sd / sqrt(complete),
           lower_qt = qt(p = (1 - conf_level) / 2,
                         df = complete - 1,
                         lower.tail = TRUE),
           upper_qt = qt(p = (1 - conf_level) / 2,
                         df = complete - 1,
                         lower.tail = FALSE),
           lower_ci = mean + lower_qt * se,
           upper_ci = mean + upper_qt * se) %>%
    dplyr::rename(group = 1) %>%
    dplyr::select(-lower_qt,
                  -upper_qt)


  #### Calculate the difference --------------------------------

  grp1 <- data %>%
    dplyr::filter(x == by_group_df[["group"]][[1]]) %>%
    dplyr::pull(y)

  grp2 <- data %>%
    dplyr::filter(x == by_group_df[["group"]][[2]]) %>%
    dplyr::pull(y)


  #### Calculate the degrees of freedom and SD --------------------------------

  ## Paired t-test ----------------
  if (paired == TRUE) {

    method <- "Paired t-test"

    if (length(grp1) != length(grp2)) {
      stop("Groups must have the same number of observations")
    }

    diff_df <- tibble::tibble(
      grp1 = grp1,
      grp2 = grp2,
      diff = grp1 - grp2)

    df <- length(diff_df$diff) - 1

    df_stmt <- glue::glue("degrees of freedom = {scales::number(x = df, accuracy = 1.0)}")


    ## Unpaired, unequal variance with Welch's degrees of freedom ----------------
  } else if (var_equal == FALSE & df_form == "Welch") {

    method <- "Two-sample t-test with unequal variances"

    n1 <- by_group_df[["n"]][[1]]
    n2 <- by_group_df[["n"]][[2]]
    s1 <- by_group_df[["sd"]][[1]]
    s2 <- by_group_df[["sd"]][[2]]

    m1 <- by_group_df[["mean"]][[1]]
    m2 <- by_group_df[["mean"]][[2]]

    A <- s1 ^ 2 / n1
    B <- s2 ^ 2 / n2

    sp2 <- A + B
    pooled_se <- sqrt(sp2)

    denom <- A ^ 2 / (n1 + 1) + B ^ 2 / (n2 + 1)

    df <- -2 + (sp2 ^ 2) / denom

    # In the case of unequal variance, the Averaged SD is calculated
    sd <- sqrt((s1 ^ 2 + s2 ^ 2) / 2)

    # t_stat <- (m1 - m2) / sqrt(A + B)
    # Incorporate mu into t-statistic calculation
    t_stat <- (m1 - m2 - mu) / sqrt(A + B)

    df_stmt <- glue::glue("{df_form}'s degrees of freedom = {scales::number(x = df, accuracy = 0.00001)}")


    ## Unpaired, unequal variance with Satterthwaite's degrees of freedom ----------------
  } else if (var_equal == FALSE & df_form == "Satterthwaite") {

    method <- "Two-sample t-test with unequal variances"

    n1 <- by_group_df[["n"]][[1]]
    n2 <- by_group_df[["n"]][[2]]
    s1 <- by_group_df[["sd"]][[1]]
    s2 <- by_group_df[["sd"]][[2]]

    m1 <- by_group_df[["mean"]][[1]]
    m2 <- by_group_df[["mean"]][[2]]

    A <- s1 ^ 2 / n1
    B <- s2 ^ 2 / n2

    sp2 <- A + B
    pooled_se <- sqrt(sp2)

    denom <- (A ^ 2) / (n1 - 1) + (B ^ 2) / (n2 - 1)

    df <- (sp2 ^ 2) / denom

    # In the case of unequal variance, the Averaged SD is calculated
    sd <- sqrt((s1 ^ 2 + s2 ^ 2) / 2)

    df_stmt <- glue::glue("{df_form}'s degrees of freedom = {scales::number(x = df, accuracy = 0.00001)}")

    ## Unpaired, equal variance ----------------
  } else if (var_equal == TRUE) {

    method <- "Two-sample t-test with equal variances"

    n1 <- by_group_df[["n"]][[1]]
    n2 <- by_group_df[["n"]][[2]]
    s1 <- by_group_df[["sd"]][[1]]
    s2 <- by_group_df[["sd"]][[2]]

    m1 <- by_group_df[["mean"]][[1]]
    m2 <- by_group_df[["mean"]][[2]]

    sp2 <- (((n1 - 1) * s1 ^ 2) + ((n2 - 1) * s2 ^ 2)) / (n1 + n2 - 2)
    pooled_se <- sqrt(sp2 * (1 / n1 + 1 / n2))

    df <- n1 + n2 - 2

    # Incorporate mu into t-statistic calculation
    t_stat <- (m1 - m2 - mu) / pooled_se

    sd <- sp2

    df_stmt <- glue::glue("degrees of freedom = {scales::number(x = df, accuracy = 1.0)}")

  }

  #### Calculate difference for summary stats --------------------------------

  if (paired == TRUE) {
    diff_res <- diff_df %>%
      summarise(group = "diff",
                n = length(diff),
                complete = sum(!is.na(diff)),
                missing = sum(is.na(diff)),
                mean = mean(diff, na.rm = TRUE),
                sd = sd(diff, na.rm = TRUE)) %>%
      mutate(se = sd / sqrt(n),
             lower_qt = qt(p = (1 - conf_level) / 2,
                           df = n - 1,
                           lower.tail = TRUE),
             upper_qt = qt(p = (1 - conf_level) / 2,
                           df = n - 1,
                           lower.tail = FALSE),
             lower_ci = mean + lower_qt * se,
             upper_ci = mean + upper_qt * se) %>%
      dplyr::rename(group = 1) %>%
      dplyr::select(-lower_qt,
                    -upper_qt)

    summary_stats <- dplyr::bind_rows(by_group_df,
                                      diff_res)

  } else {

    diff_res <- tibble::tibble(
      group = "diff",
      mean = m1 - m2,
      se = pooled_se,
      sd = sd,
      lower_qt = qt(p = (1 - conf_level) / 2,
                    df = df,
                    lower.tail = TRUE),
      upper_qt = qt(p = (1 - conf_level) / 2,
                    df = df,
                    lower.tail = FALSE),
      lower_ci = mean + lower_qt * se,
      upper_ci = mean + upper_qt * se) %>%
      dplyr::rename(group = 1) %>%
      dplyr::select(-lower_qt,
                    -upper_qt)

    summary_stats <- dplyr::bind_rows(by_group_df,
                                      combined,
                                      diff_res)


  }



  #### Perform hypothesis tests --------------------------------

  if (var_equal == FALSE & df_form == "Welch") {

    welch_gt <- tibble::tibble(alternative = c("greater"),
                               statistic = t_stat,
                               df = df,
                               estimate = m1 - m2) %>%
      mutate(se = pooled_se,
             sd = sd,
             lower_qt = qt(p = (1 - conf_level),
                           df = df,
                           lower.tail = TRUE),
             lower_ci = estimate + lower_qt * se,
             upper_ci = Inf,
             p_value = 1 - pt(q = t_stat, df = df, lower.tail = TRUE)) %>%
      dplyr::select(-lower_qt,
                    -se,
                    -sd)

    welch_lt <- tibble::tibble(alternative = c("less"),
                               statistic = t_stat,
                               df = df,
                               estimate = m1 - m2) %>%
      mutate(se = pooled_se,
             sd = sd,
             upper_qt = qt(p = (1 - conf_level),
                           df = df,
                           lower.tail = FALSE),
             lower_ci = -Inf,
             upper_ci = estimate + upper_qt * se,
             p_value = 1 - pt(q = t_stat, df = df, lower.tail = FALSE)) %>%
      dplyr::select(-upper_qt,
                    -se,
                    -sd)

    welch_2 <- tibble::tibble(alternative = c("two.sided"),
                              statistic = t_stat,
                              df = df,
                              estimate = m1 - m2) %>%
      mutate(se = pooled_se,
             sd = sd,
             lower_qt = qt(p = (1 - conf_level) / 2,
                           df = df,
                           lower.tail = TRUE),
             upper_qt = qt(p = (1 - conf_level) / 2,
                           df = df,
                           lower.tail = FALSE),
             lower_ci = estimate + lower_qt * se,
             upper_ci = estimate + upper_qt * se,
             p_value = 2 * (1 - pt(q = t_stat, df = df, lower.tail = FALSE))) %>%
      dplyr::select(-lower_qt,
                    -upper_qt,
                    -se,
                    -sd)


    hypothesis_tests <- dplyr::bind_rows(welch_2,
                                         welch_lt,
                                         welch_gt)

  } else {



    hypothesis_tests <- c("two.sided", "less", "greater") %>%
      purrr::map_df(.x = .,
                    .f = ~ t.test(x = grp1,
                                  y = grp2,
                                  alternative = .x,
                                  mu = mu,
                                  paired = paired,
                                  var.equal = var_equal,
                                  conf.level = conf_level) %>%
                      broom::tidy() %>%
                      janitor::clean_names()) %>%
      dplyr::select(alternative,
                    statistic,
                    df = parameter,
                    estimate,
                    lower_ci = conf_low,
                    upper_ci = conf_high,
                    p_value)

  }


  #### Hypothesis text for output --------------------------------

  ## Null ----------------

  if (paired == TRUE) {
    difference <- glue::glue("diff = mean({summary_stats$group[[1]]} - {summary_stats$group[[2]]})\nHo: diff = {mu}")
  } else {
    difference <- glue::glue("diff = mean({summary_stats$group[[1]]}) - mean({summary_stats$group[[2]]})\nHo: diff = {mu}")
  }

  ## Alternative ----------------

  if (show_alternative == TRUE) {

    hypothesis_tests <- hypothesis_tests %>%
      mutate(alt_hypothesis = dplyr::case_when(
        alternative == 'two.sided' ~ glue::glue('Ha: diff != {mu}'),
        alternative == 'less' ~ glue::glue('Ha: diff < {mu}'),
        alternative == 'greater' ~ glue::glue('Ha: diff > {mu}')),
        prob = c("Pr(|T| > |t|) = ",
                 "Pr(T < t) = ",
                 "Pr(T > t) = ")) %>%
      dplyr::select(alternative:upper_ci,
                    alt_hypothesis,
                    prob,
                    p_value)


  }


  #### Test for equal variance --------------------------------

  if (paired != TRUE) {

    bartlett_res <- data %>%
      with(., bartlett.test(y ~ x)) %>%
      broom::tidy(.) %>%
      janitor::clean_names() %>%
      mutate(null = "Equal variances") %>%
      dplyr::select(-parameter)

    levene_res <- data %>%
      mutate(x = factor(x)) %>%
      with(., car::leveneTest(y ~ x)) %>%
      broom::tidy() %>%
      janitor::clean_names() %>%
      dplyr::select(statistic,
                    p_value) %>%
      mutate(method = "Levene's test",
             null = "Equal variances")

    # mod_levene_res <- data %>%
    #   mutate(x = factor(x)) %>%
    #   mod_levene_test(data = .,
    #                   y = y,
    #                   x = x)

    fligner_res <- data %>%
      mutate(x = factor(x)) %>%
      with(., fligner.test(y ~ x)) %>%
      broom::tidy() %>%
      janitor::clean_names() %>%
      dplyr::select(statistic,
                    p_value) %>%
      mutate(method = "Fligner-Killeen test",
             null = "Equal variances")


    f_test <- data %>%
      mutate(x = factor(x)) %>%
      with(., var.test(y ~ x))

    f_test_res <- tibble::tibble(
      statistic = f_test$statistic,
      p_value = f_test$p.value,
      method = "F test to compare two variances",
      null = "Equal variances")

    tests_of_homogeneity_of_variance <- dplyr::bind_rows(bartlett_res,
                                                         levene_res,
                                                         # mod_levene_res,
                                                         fligner_res,
                                                         f_test_res)

    variance_check <- data %>%
      group_by(x) %>%
      summarise(n = dplyr::n(),
                skewness = skewness(y, na.rm = TRUE),
                sd = sd(y, na.rm = TRUE)) %>%
      mutate(ratio = max(by_group_df$sd) / min(by_group_df$sd),
             interpret = "Skew same direction, similar sample size, ratio < 2")

  }


  #### Results in a list --------------------------------

  result_list <- list(summary_stats = summary_stats,
                      difference = difference,
                      method = method,
                      df = df_stmt,
                      hypothesis_tests = hypothesis_tests)


  #### Additional results --------------------------------

  if (check_variance == TRUE) {

    result_list[["tests_of_homogeneity_of_variance"]] <- tests_of_homogeneity_of_variance
    result_list[["variance_check"]] <- variance_check

  }

  if (show_cohens_d == TRUE) {

    ## Calculate cohen's d ----------------

    cohens_d <- summary_stats[4, "mean"] / summary_stats[4, "sd"]

    result_list[["cohens_d"]] <- cohens_d

  }


  #### Permutation test --------------------------------

  if (include_perm) {

    # Calculate observed difference in means
    observed <- data %>%
      group_by(x) %>%
      summarise(n = dplyr::n(),
                mean = mean(y, na.rm = TRUE)) %>%
      summarise(diff_means = mean[1] - mean[2] - mu) %>%  # Subtract `mu` from the observed difference
      pull(diff_means)

    # Get the values from the data in a vector
    var_values <- data$y

    # Get the group sizes
    group_sizes <- data %>%
      group_by(x) %>%
      summarise(n = n()) %>%
      pull(n)


    # Initialize permutation results
    perm_results <- numeric(n_perms)


    for (i in seq_len(n_perms)) {
      shuffled <- sample(var_values)
      group1 <- shuffled[1:group_sizes[1]]
      group2 <- shuffled[(group_sizes[1] + 1):length(shuffled)]
      perm_results[i] <- mean(group1) - mean(group2) - mu  # Adjust for `mu`
    }


    # Calculate p-values
    two_sided <- mean(abs(perm_results) >= abs(observed))
    greater <- mean(perm_results >= observed)
    less <- mean(perm_results < observed)

    # Return Permutation Test Results

    perm_test <- tibble::tibble(
      alternative = c("two.sided",
                      "less",
                      "greater"),
      p_value_perm_test = c(two_sided,
                            less,
                            greater)

    )

    result_list[["hypothesis_tests"]] <- result_list[["hypothesis_tests"]] |>
      dplyr::left_join(perm_test,
                       by = "alternative")

    result_list[["observed_difference"]] <- observed

    # Convert perm_results into a data frame
    perm_results <- tibble::tibble(test_stat = perm_results)

    result_list[["permutation_distribution"]] <- perm_results

  }


  #### Wilcoxon test / Mann Whitney --------------------------------

  if (include_np) {

    grp1 <- data %>%
      filter(x == unique(data$x)[1]) %>%
      pull(y)
    grp2 <- data %>%
      filter(x == unique(data$x)[2]) %>%
      pull(y)

    np_res <- tibble::tibble(
      alternative = c("two.sided",
                      "less",
                      "greater")) |>
      mutate(p_value_non_param = purrr::map_dbl(.x = alternative,
                                                .f = ~ wilcox.test(x = grp1,
                                                                   y = grp2,
                                                                   mu = mu,
                                                                   paired = paired,
                                                                   alternative = .x)$p.value))


    result_list[["hypothesis_tests"]] <- result_list[["hypothesis_tests"]] |>
      dplyr::left_join(np_res,
                       by = "alternative")


  }


  #### Return a list --------------------------------

  return(result_list)

}


#' Internal function - calculates the skewness of a variable
#'
#' @param x A (non-empty) numeric vector of data values.
#'
#' @keywords internal

skewness <-  function(x, na.rm = FALSE) {

  if (na.rm) {
    x <- na.omit(x)
  }

  m3 <- mean((x - mean(x)) ^ 3)
  skewness <- m3 / (sd(x) ^ 3)
  skewness
}

#' Internal function - Conducts the modified Levene's test for homoscedastic populations.
#'
#' https://rdrr.io/cran/asbio/man/modlevene.test.html
#'
#' The modified Levene's test is a test for homoscedasticity that (unlike the
#' classic F-test) is robust to violations of normality (Conover et al. 1981).
#' In a Modified Levene's test we calculate d_{ij}=|e_{ij} - \tilde{e}_{i}|
#' where \tilde{e}_i is the ith factor level residual median. We then run an
#' ANOVA on the d_{ij}'s. If the p-value is < α, we reject the null and conclude
#' that the population error variances are not equal.
#'
#' @param data A data frame or tibble.
#' @param y A (non-empty) numeric vector of data values.
#' @param x A factor (or character) with two levels giving the corresponding groups.
#'
#' @importFrom broom augment
#' @importFrom broom tidy
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom janitor clean_names
#'
#' @keywords internal
# mod_levene_test <- function(data, y, x) {
#
#   lm1 <- with(data, lm(y ~ x))
#
#   aug_data <- broom::augment(lm1, data)
#
#
#   medians <- sapply(split(aug_data$.resid, aug_data$x), median, na.rm = TRUE)
#   resid.y <- abs(aug_data$.resid - medians[aug_data$x])
#   res <- list()
#   res <- anova(lm(resid.y ~ aug_data$x))
#
#   broom::tidy(res) %>%
#     janitor::clean_names() %>%
#     dplyr::slice(1) %>%
#     dplyr::select(statistic,
#                   p_value) %>%
#     dplyr::mutate(method = "Modified Levene's test",
#                   null = "Population error variances are equal.")
# }

