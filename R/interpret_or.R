
#' @title
#' Help with interpreting odds ratios
#'
#' @description
#' I always get tripped up interpreting odds ratios. Especially when trying to
#' make sense of results from logistic regression. This function seems to help
#' me out. Given two columns of a data frame (or tibble), it will give you a
#' list with a 2x2 table, a sample interpretation, and the odds ratio with Wald
#' confidence interval. This can then be compared to logisitc regression results
#' and make sure that thing are making sense.
#'
#' Much of this is owed to the \href{https://exploringdatablog.blogspot.com/2011/05/computing-odds-ratios-in-r.html}{ExploringDataBlog}
#'
#' @param data A tibble or data frame.
#' @param x The "X" variable of interest. Appears along the vertical (left) side
#'   of the 2x2 table. This would be the predictor in a logistic regression.
#'   Typically considered the "Exposure" in Epidemiology.
#' @param y The "Y" variable. Appears along the horizontal (top) side of the 2x2
#'   table. THis would be the outcome in a logistic regression. In Epidemiology,
#'   this would be the case/control status or the disease status.
#' @param alpha Default = 0.05. The significance level for the two-sided Wald
#'   confidence interval.
#'
#' @references
#' https://exploringdatablog.blogspot.com/2011/05/computing-odds-ratios-in-r.html
#'
#' https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
#'
#'
#' @import rlang
#' @importFrom broom tidy
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom tibble tibble
#'
#' @return
#' A list with the following:
#' \describe{
#'   \item{table}{2x2 contingency table}
#'   \item{interpretation}{Sample interpretation of the odds ratio of the
#'   outcome and the exposure levels}
#'   \item{results}{Odds ratio and Wald confidence interval}
#'   \item{fishers}{Results of Fisher's test}
#'   \item{chisq}{Results of Chi-square test}
#' }
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(forcats)
#' library(readr)
#' library(broom)
#'
#' #### Example 1 --------------------------------
#' mydata <- admissions
#' mydata <- mydata %>%
#'   mutate(rank = factor(rank),
#'          rank = forcats::fct_collapse(rank,
#'                                       "1" = c("1", "2"),
#'                                       "2" = c("3", "4")),
#'          admit = factor(admit,
#'                         levels = c(1, 0),
#'                         labels = c("Yes", "No")))
#'
#' glm((admit == "Yes") ~ rank,
#'     data = mydata,
#'     family = binomial(link = "logit")) %>%
#'   broom::tidy(., exponentiate = TRUE)
#'
#' interpret_or(data = mydata,
#'              x = rank,
#'              y = admit)
#' # We see here that the odds ratio is flipped and the interpretation is not
#' # consistent with the reference group in the logistic regression results above.
#' # So let's fix it.
#'
#' mydata %>%
#'   mutate(rank = forcats::fct_rev(rank)) %>%
#'   interpret_or(data = .,
#'                x = rank,
#'                y = admit)
#'
#' # Now things match!! Remember this is just supposed to help understand and
#' # interpret the results. Stay mindful of the reference groups!
#'
#' #### Example 2 --------------------------------
#'
#' dis_df <- tibble::tibble(
#'   Outcome = sample(c("Diseased", "Non-diseased"),
#'                    size = 100,
#'                    replace = TRUE,
#'                    prob = c(0.25, 0.75)),
#'   Exposure = sample(c("Exposed", "Unexposed"),
#'                    size = 100,
#'                    replace = TRUE,
#'                    prob = c(0.40, 0.60))) %>%
#'   mutate_all(.tbl = .,
#'              .funs = list(~ factor(.)))
#'
#'
#' interpret_or(data = dis_df,
#'              x = Exposure,
#'              y = Outcome)
#'
#'
#' #### Example 3 --------------------------------
#'
#' sample_df <- hsb_sample
#' sample_df <- sample_df %>%
#'   mutate(female = factor(female,
#'                          levels = c(0, 1),
#'                          labels = c("male", "female")))
#'
#' xtabs(~ female + hon,
#'       data = sample_df)
#'
#' glm((hon == 1) ~ female,
#'     data = sample_df,
#'     family = binomial(link = "logit")) %>%
#'   broom::tidy(., exponentiate = TRUE)
#'
#'
#' sample_df %>%
#'   mutate(female = forcats::fct_rev(female),
#'          hon = factor(hon,
#'                       levels = c(1, 0))) %>%
#'   interpret_or(data = .,
#'                x = female,
#'                y = hon)


interpret_or <- function(data, x, y, alpha = 0.05) {

  xtab <- data %>%
    dplyr::select({{ x }}, {{ y }}) %>%
    table(.)

  n00 <- xtab[1, 1]
  n01 <- xtab[1, 2]
  n10 <- xtab[2, 1]
  n11 <- xtab[2, 2]

  fisher_res <- fisher.test(xtab) %>%
    broom::tidy() %>%
    janitor::clean_names()

  chisq_res <- chisq.test(xtab) %>%
    broom::tidy() %>%
    janitor::clean_names()

  out_list <- vector("list", 5)
  out_list[[1]] <- xtab
  out_list[[2]] <- glue::glue("The odds of [ {names(dimnames(xtab)[2])} = {dimnames(xtab)[[2]][1]} ] among those with [ {names(dimnames(xtab)[1])} = {dimnames(xtab)[[1]][1]} ] is x times the odds of those with [ {names(dimnames(xtab)[1])} = {dimnames(xtab)[[1]][2]} ]")
  out_list[[3]] <- calc_or_wald(n00, n01, n10, n11, alpha)
  out_list[[4]] <- fisher_res
  out_list[[5]] <- chisq_res
  names(out_list) <- c("table", "interpretaion", "results", "fishers", "chisq")
  out_list

}



#### calc_or_wald --------------------------------

# A helper function
# https://exploringdatablog.blogspot.com/2011/05/computing-odds-ratios-in-r.html

calc_or_wald <- function(n00, n01, n10, n11, alpha = 0.05) {
  #
  #  Compute the odds ratio between two binary variables, x and y,
  #  as defined by the four numbers nij:
  #
  #    n00 = number of cases where x = 0 and y = 0
  #    n01 = number of cases where x = 0 and y = 1
  #    n10 = number of cases where x = 1 and y = 0
  #    n11 = number of cases where x = 1 and y = 1
  #
  OR <- (n00 * n11) / (n01 * n10)
  #
  #  Compute the Wald confidence intervals:
  #
  siglog <- sqrt((1 / n00) + (1 / n01) + (1 / n10) + (1 / n11))
  zalph <- qnorm(1 - alpha / 2)
  logOR <- log(OR)
  loglo <- logOR - zalph * siglog
  loghi <- logOR + zalph * siglog
  #
  ORlo <- exp(loglo)
  ORhi <- exp(loghi)
  #
  oframe <- tibble::tibble(odds_ratio = OR,
                           lower_ci = ORlo,
                           upper_ci = ORhi,
                           alpha = alpha)
  oframe
}
