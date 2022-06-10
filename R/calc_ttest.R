

#' @title
#' Calculate t-test
#'
#' @description
#' Perform two sample t-tests (one sample coming later). Wrapper around `t.test`
#' to return cleaner results and more information in one function.
#'
#' @param data A tibble or data frame.
#' @param var A numeric vector of data values.
#' @param by Factor or character indicating groups
#' @param mu a number indicating the true value of the mean (or difference in means if you are performing a two sample test).
#' @param paired a logical indicating whether you want a paired t-test.
#' @param var_equal a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param df_form Character string indicating the formula to use to calculate the degrees of freedom when the variances are unequal: "Welch" (default) or "Satterthwaite"
#' @param conf_level confidence level of the interval.
#' @param check_variance If TRUE returns checks on the Hogeneity of variances. Default is FALSE
#' @param reverse_groups If TRUE, the `by` groups are reversed
#' @param show_cohens_d If TRUE, then estimated Cohen's d (effect size) is returned.
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
#' @importFrom rlang expr
#' @importFrom rlang get_expr
#' @importFrom rlang quo_name
#' @importFrom stats bartlett.test
#' @importFrom stats t.test
#' @importFrom tibble tibble
#'
#' @return
#' A list
#'
#' @export
#'
#' @examples
#'
#' library(tibble)
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
#' # Unpaired, unequal variances, Welch DF
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            mu = 0,
#'            paired = FALSE,
#'            var_equal = FALSE,
#'            conf_level = 0.95)
#'
#' # Unpaired, unequal variances, Satterthwaite DF
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            mu = 0,
#'            paired = FALSE,
#'            var_equal = FALSE,
#'            df_form = "Satterthwaite",
#'            conf_level = 0.95)
#'
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            mu = 0,
#'            paired = FALSE,
#'            var_equal = TRUE,
#'            conf_level = 0.95)
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            mu = 0,
#'            paired = FALSE,
#'            var_equal = TRUE,
#'            conf_level = 0.95,
#'            show_cohens_d = TRUE)
#'
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            mu = 0,
#'            paired = FALSE,
#'            var_equal = TRUE,
#'            conf_level = 0.95,
#'            reverse_groups = TRUE)
#'
#' calc_ttest(data = fuel,
#'            var = mpg,
#'            by = treated,
#'            mu = 0,
#'            paired = FALSE,
#'            var_equal = FALSE,
#'            conf_level = 0.95,
#'            check_variance = TRUE)
#'
#'
#'
#'
#' ### --------------------------------------------------------------
#' ### Paired t-test, Flicker feather example, p. 185
#' ### --------------------------------------------------------------
#'
#' feather <- tibble::tribble(
#'   ~Bird,     ~name, ~value,
#'   "A", "Typical", -0.255,
#'   "A",     "Odd", -0.324,
#'   "B", "Typical", -0.213,
#'   "B",     "Odd", -0.185,
#'   "C", "Typical",  -0.19,
#'   "C",     "Odd", -0.299,
#'   "D", "Typical", -0.185,
#'   "D",     "Odd", -0.144,
#'   "E", "Typical", -0.045,
#'   "E",     "Odd", -0.027,
#'   "F", "Typical", -0.025,
#'   "F",     "Odd", -0.039,
#'   "G", "Typical", -0.015,
#'   "G",     "Odd", -0.264,
#'   "H", "Typical",  0.003,
#'   "H",     "Odd", -0.077,
#'   "I", "Typical",  0.015,
#'   "I",     "Odd", -0.017,
#'   "J", "Typical",   0.02,
#'   "J",     "Odd", -0.169,
#'   "K", "Typical",  0.023,
#'   "K",     "Odd", -0.096,
#'   "L", "Typical",   0.04,
#'   "L",     "Odd",  -0.33,
#'   "M", "Typical",   0.04,
#'   "M",     "Odd", -0.346,
#'   "N", "Typical",   0.05,
#'   "N",     "Odd", -0.191,
#'   "O", "Typical",  0.055,
#'   "O",     "Odd", -0.128,
#'   "P", "Typical",  0.058,
#'   "P",     "Odd", -0.182
#' )
#'
#'
#'
#' calc_ttest(data = feather,
#'            var = value,
#'            by = name,
#'            paired = TRUE)


calc_ttest <- function(data,
                       var,
                       by = NULL,
                       mu = 0,
                       paired = TRUE,
                       var_equal = FALSE,
                       df_form = "Welch",
                       conf_level = 0.95,
                       check_variance = FALSE,
                       reverse_groups = FALSE,
                       show_cohens_d = FALSE) {

  #### Notes --------------------------------

  # 2021-08-16
  # Works for paired t-test
  # https://www.datanovia.com/en/lessons/t-test-effect-size-using-cohens-d-measure/

  group_var <- rlang::enquo(by)
  var <- rlang::enquo(var)


  if (reverse_groups) {

    data <- data %>%
      mutate(!! rlang::quo_name(group_var) := as.factor(!! group_var),
             !! rlang::quo_name(group_var) := forcats::fct_rev(!! group_var))

  }

  #### Summary stats by group --------------------------------

  by_group_df <- data %>%
    group_by(!! group_var) %>%
    summarise(n = length(!! var),
              complete = sum(!is.na(!! var)),
              missing = sum(is.na(!! var)),
              mean = mean(!! var, na.rm = TRUE),
              sd = sd(!! var, na.rm = TRUE),
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
    ungroup()

  #### Summary stats for all combined --------------------------------

  combined <- data %>%
    summarise(group = "combined",
              n = length(!! var),
              complete = sum(!is.na(!! var)),
              missing = sum(is.na(!! var)),
              mean = mean(!! var, na.rm = TRUE),
              sd = sd(!! var, na.rm = TRUE)) %>%
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
    dplyr::filter(!! group_var == by_group_df[["group"]][[1]]) %>%
    dplyr::pull(!! var)

  grp2 <- data %>%
    dplyr::filter(!! group_var == by_group_df[["group"]][[2]]) %>%
    dplyr::pull(!! var)

  if (paired == TRUE) {

    if (length(grp1) != length(grp2)) {
      stop("Groups must have the same number of observations")
    }

    diff_df <- tibble::tibble(
      grp1 = grp1,
      grp2 = grp2,
      diff = grp1 - grp2)

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

    df <- length(diff_df$diff) - 1

  } else {

    n1 <- by_group_df[["n"]][[1]]
    n2 <- by_group_df[["n"]][[2]]
    s1 <- by_group_df[["sd"]][[1]]
    s2 <- by_group_df[["sd"]][[2]]

    m1 <- by_group_df[["mean"]][[1]]
    m2 <- by_group_df[["mean"]][[2]]


    if (var_equal == FALSE & df_form == "Welch") {

      v1 <- (s1 ^ 2)
      v2 <- (s2 ^ 2)

      sp2 <- v1 / n1 + v2 / n2
      pooled_se <- sqrt(sp2)

      denom <- ((v1 / n1) ^ 2) / (n1 + 1) + ((v2 / n2) ^ 2) / (n2 + 1)

      df <- -2 + (sp2 ^ 2) / denom
      # df <- trunc(df)

      # In the case of unequal variance, the Averaged SD is calculated
      sd <- sqrt((s1 ^ 2 + s2 ^ 2) / 2)

    } else if (var_equal == FALSE & df_form == "Satterthwaite") {

      v1 <- (s1 ^ 2)
      v2 <- (s2 ^ 2)

      sp2 <- v1 / n1 + v2 / n2
      pooled_se <- sqrt(sp2)

      denom <- ((v1 / n1) ^ 2) / (n1 - 1) + ((v2 / n2) ^ 2) / (n2 - 1)

      df <- (sp2 ^ 2) / denom
      # df <- trunc(df)

      # In the case of unequal variance, the Averaged SD is calculated
      sd <- sqrt((s1 ^ 2 + s2 ^ 2) / 2)

    } else if (var_equal == TRUE) {

      sp2 <- (((n1 - 1) * s1 ^ 2) + ((n2 - 1) * s2 ^ 2)) / (n1 + n2 - 2)
      pooled_se <- sqrt(sp2 * (1 / n1 + 1 / n2))

      df <- n1 + n2 - 2

      sd <- sp2

    }

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


  hypothesis_tests <- c("two.sided", "less", "greater") %>%
    purrr::map_df(.x = .,
                  .f = ~ stats::t.test(x = grp1,
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
                  p_value) %>%
    # mutate(p_value = lamisc::fmt_pvl(p_value)) %>%
    {.}

  method <- dplyr::case_when(
    paired == "TRUE" ~ "Paired t-test",
    var_equal == "TRUE" ~ "Two-sample t-test with equal variances",
    var_equal == "FALSE" ~ "Two-sample t-test with unequal variances"
  )

  if (paired == TRUE) {
    difference <- glue::glue("diff = mean({summary_stats$group[[1]]} - {summary_stats$group[[2]]})")
  } else {
    difference <- glue::glue("diff = mean({summary_stats$group[[1]]}) - mean({summary_stats$group[[2]]})")
  }


  #### Test for equal variance --------------------------------

  if (paired != TRUE) {

    bartlett_res <- data %>%
      # mutate(!! rlang::quo_name(group_var) := forcats::fct_drop(!! group_var)) %>%
      with(., stats::bartlett.test(as.formula(
        rlang::expr(!! rlang::get_expr(var) ~ !! rlang::get_expr(group_var))))) %>%
      broom::tidy(.) %>%
      janitor::clean_names() %>%
      mutate(null = "Equal variances") %>%
      dplyr::select(-parameter)

    levene_res <- data %>%
      mutate(!! rlang::quo_name(group_var) := factor(!! group_var)) %>%
      with(., car::leveneTest(as.formula(
        rlang::expr(!! rlang::get_expr(var) ~ !! rlang::get_expr(group_var))))) %>%
      broom::tidy() %>%
      janitor::clean_names() %>%
      dplyr::select(statistic,
                    p_value) %>%
      mutate(method = "Levene's test",
             null = "Equal variances")

    fligner_res <- data %>%
      mutate(!! rlang::quo_name(group_var) := factor(!! group_var)) %>%
      with(., stats::fligner.test(as.formula(
        rlang::expr(!! rlang::get_expr(var) ~ !! rlang::get_expr(group_var))))) %>%
      broom::tidy() %>%
      janitor::clean_names() %>%
      dplyr::select(statistic,
                    p_value) %>%
      mutate(method = "Fligner-Killeen test",
             null = "Equal variances")

    tests_of_homogeneity_of_variance <- dplyr::bind_rows(bartlett_res,
                                                         levene_res,
                                                         fligner_res)

    variance_check <- data %>%
      group_by(!! group_var) %>%
      summarise(n = dplyr::n(),
                skewness = skewness(!! var, na.rm = TRUE),
                sd = sd(!! var, na.rm = TRUE)) %>%
      mutate(ratio = max(by_group_df$sd) / min(by_group_df$sd),
             interpret = "Skew same direction, similar sample size, ratio < 2")

    var_check_list <- list(tests_of_homogeneity_of_variance = tests_of_homogeneity_of_variance,
                           variance_check = variance_check)

  }

  #### Calculate cohen's d --------------------------------

  cohens_d <- summary_stats[4, "mean"] / summary_stats[4, "sd"]


  #### Results in a list --------------------------------

  result_list <- list(df = df,
         summary_stats = summary_stats,
         difference = difference,
         method = method,
         hypothesis_tests = hypothesis_tests)


  #### Additional results --------------------------------

  if (check_variance == TRUE) {


    # result_list["tests_of_homogeneity_of_variance"] <- tests_of_homogeneity_of_variance
    # result_list["variance_check"] <- variance_check

    result_list <- append(result_list, var_check_list)

  }

  if (show_cohens_d == TRUE) {

    result_list["cohens_d"] <- cohens_d

    }


  #### Return a list --------------------------------

  return(result_list)

}



#### Helper functions --------------------------------


skewness <-  function(x, na.rm = FALSE) {

  if (na.rm) {
    x <- na.omit(x)
  }

  m3 <- mean((x - mean(x))^3)
  skewness <- m3/(sd(x)^3)
  skewness
}


