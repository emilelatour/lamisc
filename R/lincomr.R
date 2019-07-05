#' @title
#' Calculate a linear combination from regression results.
#'
#' @description
#' Pretty simple and strighforward right now. My friend Mike Lasarev had a big
#' hand in helping me get this set up and figuring out how to do it.
#'
#' It should handle results from linear regression, mixed models, logistic
#' regression, and survival analysis. The big drawback currently is that
#' _p_-values and test statistics are based on asymptotic theory; in the future
#' it would be a better idea to incorporate t-statistics or Satterthwaite
#' degrees of freedom.
#'
#' @import dplyr
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#' @importFrom stats vcov
#' @importFrom rlang .data
#' @importFrom tibble tibble
#'
#' @param k A vector of 1's and 0's to indicate the linear combination
#' @param fit A model fit object
#' @param alpha Significance level
#'
#' @return A data frame of results
#' @export
#'
#' @examples
#' data(iris)
#' mod_1 <- lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length,
#'             data = iris)
#' summary(mod_1)
#' lincomr(k = c(1, 0, 1, 1), fit = mod_1)
lincomr <- function(k, fit, alpha = 0.05) {

  k <- matrix(k, nrow = 1)

  # get the beta coefficients
  # bhat <- matrix(fit@beta, ncol = 1)
  bhat <- matrix(summary(fit)$coefficient[, 1], ncol = 1)

  # Get the variance covariance matrix
  v <- as.matrix(stats::vcov(fit))

  # Calculate the standard error
  se <- sqrt(k %*% v %*% t(k))

  # estimate the value of the linear combination
  est <- k %*% bhat


  ## TODO ---------------
  # This should probably use the t-statistic rather than the z. Something for
  # another time I think.
  # t_st <- (-6.4796 - 0) / 2.2785
  # t_st
  # 2 * pt(t_st, df = 140 - 3 - 1)
  # # df = n - p - 1
  # pval <- 2 * pt(t_st, df = summary(rat_fit)$df.residual - 1)
  # pval

  # Determine the z-score
  z_score <- (est - 0) / se

  # Calculate the p-value
  p_value <- 2 * (1 - stats::pnorm(q = z_score,
                                   mean = 0,
                                   sd = 1,
                                   lower.tail = TRUE,
                                   log.p = FALSE))

  p_value <- as.double(p_value)

  # determine the z-statistic for the 95% CI
  z_stat <- stats::qnorm(p = 1 - alpha / 2,
                         mean = 0,
                         sd = 1,
                         lower.tail = TRUE,
                         log.p = FALSE)

  # Upper and Lower 95% CIs
  lower_ci <- est - z_stat * se
  upper_ci <- est + z_stat * se


  #### Return a data frame --------------------------------

  tibble::tibble("coef" = est[[1]],
             "std_err" = se[[1]],
             "z_stat" = z_score[[1]],
             "p_value" = lamisc::fmt_pvl(p_value),
             "lower_ci" = lower_ci[[1]],
             "upper_ci" = upper_ci[[1]]) %>%
    mutate_at(.tbl = .,
              .vars = vars(.data$coef,
                           .data$std_err,
                           .data$z_stat,
                           .data$lower_ci,
                           .data$upper_ci),
              .funs = list(~ lamisc::fmt_num(x = .,
                                             accuracy = 0.01,
                                             as_numeric = FALSE)))

}

