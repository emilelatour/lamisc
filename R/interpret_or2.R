
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
#'   c("Exposed", "Unexposed"), outcome_status = c("Positive", "Negative"))`
#' @param alpha Default = 0.05. The significance level for the two-sided Wald
#'   confidence interval.
#'
#' @references
#' https://exploringdatablog.blogspot.com/2011/05/computing-odds-ratios-in-r.html
#'
#' https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
#'
#'
#' @importFrom rlang sym
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
#' library(broom)
#' library(janitor)
#'
#' # Example 1: Interpreting a 2x2 table
#' interpret_or2(a = 40, b = 148, c = 87, d = 125,
#'               dim_names = list(rank = c("2", "1"),
#'                                admit = c("Yes", "No")))
#'
#' # Example 2: Using a contingency table from data
#' dis_df <- tibble::tibble(
#'   Outcome = sample(c("Diseased", "Non-diseased"), 100, TRUE, c(0.25, 0.75)),
#'   Exposure = sample(c("Exposed", "Unexposed"), 100, TRUE, c(0.40, 0.60))
#' )
#' janitor::tabyl(dis_df, Exposure, Outcome)
#' interpret_or2(a = 11, b = 28, c = 15, d = 46,
#'               dim_names = list(Exposure = c("Exposed", "Unexposed"),
#'                                Outcome = c("Diseased", "Non-diseased")))


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

  fisher_res <- fisher.test(xtab) |>
    broom::tidy() |>
    janitor::clean_names()

  chisq_res <- chisq.test(xtab) |>
    broom::tidy() |>
    janitor::clean_names()

  out_list <- vector("list", 5)
  out_list[[1]] <- input_table
  out_list[[2]] <- glue::glue("The odds of [ {names(df)[[2]]} = {dimnames(xtab)[[2]][1]} ] among those with [ {names(df)[[1]]} = {dimnames(xtab)[[1]][1]} ] is x times the odds of those with  [ {names(df)[[1]]} = {dimnames(xtab)[[1]][2]} ]")
  out_list[[3]] <- calc_or_wald(n00, n01, n10, n11, alpha)
  out_list[[4]] <- fisher_res
  out_list[[5]] <- chisq_res
  names(out_list) <- c("table", "interpretaion", "results", "fishers", "chisq")
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
