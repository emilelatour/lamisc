
#' @title
#' Calculate agreement statistics for a 2 x 2 table
#'
#' @description
#' I have opinions about what statistics should be reported along with kappa
#' when asssessing agreement between two raters. The chosen stats are based on
#' the advice in paper a paper by **reference needed**.
#'
#'
#' @param table A table
#' @param has_gold_std Logical; `TRUE` will show the sensitivity and specificity
#'   using rater __ as the gold standard. `FALSE` assumes there is no gold
#'   standard and calculated the proportions of positive and negative agreement
#'   instead.
#'
#' @import dplyr
#' @import rlang
#' @import tibble
#' @importFrom psych cohen.kappa
#' @importFrom rlang .data
#'
#' @return A tibble or data frame
#' @export
#'
#' @examples
#' ## Example 1 ----------------
#'
#' foo <- matrix(c(20.61, 1.52, 9.58, 68.29), ncol = 2, byrow = TRUE)
#' foo <- as.table(100 * foo)
#' foo
#'
#' calc_2x2_res_table(table = foo, has_gold_std = FALSE)
#' calc_2x2_res_table(table = foo, has_gold_std = TRUE)
#'
#' ## Example 2 ----------------
#'
#' df <- tibble::tribble(
#'   ~a, ~b, ~c,
#'   1L, 1L, 1L,
#'   0L, 1L, 1L,
#'   1L, 1L, 0L,
#'   0L, 1L, 0L,
#'   1L, 1L, 1L,
#'   0L, 1L, 1L,
#'   1L, 1L, 0L
#' )
#'
#' bar <- lamisc::make_table(df = df,
#'                           x_var = a,
#'                           x_lvls = c("1", "0"),
#'                           y_var = b,
#'                           y_lvls = c("1", "0"))
#'
#' calc_2x2_res_table(table = bar, has_gold_std = FALSE)
#' calc_2x2_res_table(table = bar, has_gold_std = TRUE)


calc_2x2_res_table <- function(table, has_gold_std = FALSE) {

  if (dim(table)[[1]] != 2)
    stop("Table must have dim = 2.")
  if (dim(table)[[1]] != dim(table)[[1]])
    stop("Table must have same dimensions.")

  if (has_gold_std == FALSE) {

    result <-
      tibble::tibble(p1 = calc_prev(table, rater = 1),
                     p2 = calc_prev(table, rater = 2),
                     a = calc_abcdn(table)$a,
                     b = calc_abcdn(table)$b,
                     c = calc_abcdn(table)$c,
                     d = calc_abcdn(table)$d,
                     pabak = calc_pabak(table),
                     kappa = psych::cohen.kappa(x = table)$kappa,
                     pos_agree = calc_prop_agree(table, agree = "positive"),
                     neg_agree = calc_prop_agree(table, agree = "negative")
      ) %>%
      dplyr::mutate_at(.vars = vars(.data$p1:.data$d),
                       .funs = funs(lamisc::roundr(., d = 1))) %>%
      dplyr::mutate_at(.vars = vars(.data$pabak:.data$neg_agree),
                       .funs = funs(lamisc::roundr(., d = 3)))

  } else if (has_gold_std == TRUE) {

    result <-
      tibble::tibble(p1 = calc_prev(table, rater = 1),
                     p2 = calc_prev(table, rater = 2),
                     a = calc_abcdn(table)$a,
                     b = calc_abcdn(table)$b,
                     c = calc_abcdn(table)$c,
                     d = calc_abcdn(table)$d,
                     pabak = calc_pabak(table),
                     kappa = psych::cohen.kappa(x = table)$kappa,
                     sens = calc_sens_spec(table, choose_stat = "sensitivity"),
                     spec = calc_sens_spec(table, choose_stat = "specificity")
      ) %>%
      dplyr::mutate_at(.vars = vars(.data$p1:.data$d),
                       .funs = funs(lamisc::roundr(., d = 1))) %>%
      dplyr::mutate_at(.vars = vars(.data$pabak:.data$spec),
                       .funs = funs(lamisc::roundr(., d = 3)))

  }


  return(result)

}




#### Helper functions called by TBD --------------------------------

## calc_prev ----------------
# Calculate the prevalence determined by each rater, i.e. the positive rating
# by each rater.
# Takes a table as an input

calc_prev <- function(input.table, rater = 1) {

  n <- sum(input.table)   # Total eligible patients
  a <- input.table[1, 1]  # "Yes" in both sources
  b <- input.table[1, 2]  # "Yes" in source 1, on the vertical, "No" in other
  c <- input.table[2, 1]  # "Yes" in source 2, on the horizontal, "No" in other
  d <- input.table[2, 2]  # "No" in both sources

  if (rater == 1) {
    prev <- (a + c) / n
  } else if (rater == 2) {
    prev <- (a + b) / n
  }

  return(100 * prev)

}

# Example of use
# foo <- matrix(c(20.61, 1.52, 9.58, 68.29), ncol = 2, byrow = TRUE)
# foo <- as.table(100 * foo)
# calc_prev(foo, rater = 1)
# calc_prev(foo, rater = 2)


## calc_abcdn ----------------

# Takes a table as an input and puts the cell % in a list

calc_abcdn <- function(input.table) {

  n <- sum(input.table)   # Total eligible patients
  a <- input.table[1, 1]  # "Yes" in both sources
  b <- input.table[1, 2]  # "Yes" in source 1, on the vertical, "No" in other
  c <- input.table[2, 1]  # "Yes" in source 2, on the horizontal, "No" in other
  d <- input.table[2, 2]  # "No" in both sources

  n <- n
  a <- 100 * (a / n)
  b <- 100 * (b / n)
  c <- 100 * (c / n)
  d <- 100 * (d / n)

  list(n = n, a = a, b = b, c = c, d = d)

}

# Example of use
# foo <- matrix(c(20.61, 1.52, 9.58, 68.29), ncol = 2, byrow = TRUE)
# foo <- as.table(100 * foo)
# calc_abcdn(foo)


## calc_pabak ----------------

# calculate the prevalence and bias adjusted kappa (PABAK)
# takes a table as an input

calc_pabak <- function(input.table) {

  n <- sum(input.table)   # Total eligible patients
  a <- input.table[1, 1]  # "Yes" in both sources
  b <- input.table[1, 2]  # "Yes" in source 1, on the vertical, "No" in other
  c <- input.table[2, 1]  # "Yes" in source 2, on the horizontal, "No" in other
  d <- input.table[2, 2]  # "No" in both sources

  # Observed proportion of agreement
  Po <- (a + d) / n

  # Expected proportion of agreement
  Pe <- ((a + c) / n * (a + b) / n) + ((b + d) / n * (c + d) / n)

  # Prevalence index
  PI <- abs(a / n - d / n)

  # Bias index
  BI <- abs((a + b) / n - (a + c) / n)

  # PABAK calc
  PABAK <- 2 * Po - 1
  # PABAK <- K * (1 - PI^2 + BI^2) + PI^2 - BI^2
  return(PABAK)

}

# Example of use
# foo <- matrix(c(20.61, 1.52, 9.58, 68.29), ncol = 2, byrow = TRUE)
# foo <- as.table(100 * foo)
# calc_pabak(foo)


## calc_prop_agree ----------------

# calculate the proportion of positive agreement and the proportion of negative
# agreement
# takes a table as an input

calc_prop_agree <- function(input.table, agree = NULL) {

  n <- sum(input.table)   # Total eligible patients
  a <- input.table[1, 1]  # "Yes" in both sources
  b <- input.table[1, 2]  # "Yes" in source 1, on the vertical, "No" in other
  c <- input.table[2, 1]  # "Yes" in source 2, on the horizontal, "No" in other
  d <- input.table[2, 2]  # "No" in both sources

  # Proportion of positive agreement
  Ppos <- (2 * a) / (n + a - d)

  # Proportion of negative agreement
  Pneg <- (2 * d) / (n - a + d)

  Pagree <- list(Ppos = Ppos, Pneg = Pneg)

  if (is.null(agree)) {
    return(Pagree)
  } else if (agree == "positive") {
    return(Pagree$Ppos)
  } else if (agree == "negative") {
    return(Pagree$Pneg)
  }


}

# Example of use
# foo <- matrix(c(20.61, 1.52, 9.58, 68.29), ncol = 2, byrow = TRUE)
# foo <- as.table(100 * foo)
# calc_prop_agree(foo, agree = "positive")
# calc_prop_agree(foo, agree = "negative")
# calc_prop_agree(foo)


## calc_sens_spec ----------------

# calculate the proportion of positive agreement and the proportion of negative
# agreement
# takes a table as an input

calc_sens_spec <- function(input.table, choose_stat = NULL) {

  n <- sum(input.table)   # Total eligible patients
  a <- input.table[1, 1]  # "Yes" in both sources
  b <- input.table[1, 2]  # "Yes" in source 1, on the vertical, "No" in other
  c <- input.table[2, 1]  # "Yes" in source 2, on the horizontal, "No" in other
  d <- input.table[2, 2]  # "No" in both sources

  # Sensitivity
  sens <- a / (a + c)

  # Specificity
  spec <- d / (b + d)

  result <- list(sensitivity = sens, specificity = spec)

  if (is.null(choose_stat)) {
    return(result)
  } else if (choose_stat == "sensitivity") {
    return(result$sensitivity)
  } else if (choose_stat == "specificity") {
    return(result$specificity)
  }


}

# Example of use
# foo <- matrix(c(20.61, 1.52, 9.58, 68.29), ncol = 2, byrow = TRUE)
# foo <- as.table(100 * foo)
# calc_sens_spec(foo, choose_stat = "sensitivity")
# calc_sens_spec(foo, choose_stat = "specificity")
# calc_sens_spec(foo)
