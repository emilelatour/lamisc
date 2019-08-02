
#' @title
#' Help with interpreting odds ratios (using 2x2 table values)
#'
#' @description
#' I always get tripped up interpreting odds ratios. Especially when trying to
#' make sense of results from logistic regression. This function seems to help
#' me out.
#'
#' Unlike `lamsic::interpret_or`, this function take the cells (`a`, `b`, `c`,
#' and `d`) of a 2x2 contingency table as inputs directly rather than columns of
#' a data frame.
#'
#' The function returns a list with a 2x2 table, a sample interpretation, and
#' the odds ratio with Wald confidence interval. This can then be compared to
#' logisitc regression results and make sure that thing are making sense.
#'
#' Much of this is owed to the \href{https://exploringdatablog.blogspot.com/2011/05/computing-odds-ratios-in-r.html}{ExploringDataBlog}
#'
#' @param a Count of the upper left quadrant of 2x2 contingency table
#' @param b Count of upper right
#' @param c Count of lower left
#' @param d Count of lower right
#' @param dim_names A list; labels or the rows and columns of the 2x2
#'   contingency table. Default is `dim_names = list(exposure_status =
#'   c("Exposed", "Unexposed"), outcome_status = c("Postive", "Negative"))`
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
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom tibble tibble
#'
#' @return
#' A list with the following:
#' \describe{
#'   \item{table}{2x2 contingency table}
#'   \item{interpretation}{Sample interpretation of the odds ratio of the
#'   outcome and the exposure levels}
#'   \item{results}{Odds ratio and Wald confidence interval}
#' }
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(forcats)
#' library(readr)
#' library(broom)
#' library(janitor)
#'
#' #### Example 1 --------------------------------
#' mydata <- readr::read_csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
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
#' # Get a 2x2 table
#' janitor::tabyl(dat = mydata,
#'                rank,
#'                admit)
#'
#' # Note that I flip the values to match the refernce level in the logistic
#' # regression
#' interpret_or2(a = 40,
#'               b = 148,
#'               c = 87,
#'               d = 125,
#'               dim_names = list(rank = c("2", "1"),
#'                                admit = c("Yes", "No")))
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
#' # Get a 2x2 table
#' janitor::tabyl(dat = dis_df,
#'                Exposure,
#'                Outcome) %>%
#'   janitor::adorn_title(placement = "combined")
#'
#'
#' interpret_or2(a = 11,
#'               b = 28,
#'               c = 15,
#'               d = 46,
#'               dim_names = list(Exposure = c("Exposed", "Unexposed"),
#'                                Outcome = c("Diseased", "Non-diseased")))
#'
#'
#' #### Example 3 --------------------------------
#'
#' sample_df <- readr::read_csv("https://stats.idre.ucla.edu/wp-content/uploads/2016/02/sample.csv")
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
#' interpret_or2(a = 32,
#'               b = 77,
#'               c = 17,
#'               d = 74,
#'               dim_names = list(Sex = c("Female", "Male"),
#'                                Honors = c("Yes", "No")))


interpret_or2 <- function(a, b, c, d,
                         dim_names = list(exposure_status = c("Exposed", "Unexposed"),
                                          outcome_status = c("Postive", "Negative")),
                         alpha = 0.05) {

  input_table <- matrix(c(a, b, c, d), byrow = TRUE, ncol = 2)
  dimnames(input_table) <- dim_names

  df <- lamisc::counts_to_cases(input_table = input_table)

  xtab <- table(df[[1]], df[[2]])
  n00 <- xtab[1, 1]
  n01 <- xtab[1, 2]
  n10 <- xtab[2, 1]
  n11 <- xtab[2, 2]

  out_list <- vector("list", 3)
  out_list[[1]] <- input_table
  out_list[[2]] <- glue::glue("The odds of [ {names(df)[[2]]} = {dimnames(xtab)[[2]][1]} ] among those with [ {names(df)[[1]]} = {dimnames(xtab)[[1]][1]} ] is x times the odds of those with  [ {names(df)[[1]]} = {dimnames(xtab)[[1]][2]} ]")
  out_list[[3]] <- calc_or_wald(n00, n01, n10, n11, alpha)
  names(out_list) <- c("table", "interpretaion", "results")
  out_list

}



#### calc_or_wald --------------------------------

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
