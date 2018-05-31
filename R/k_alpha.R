# input:
# ratings_t = data set
# alpha_q = two-sided type one error, default = 0.05
# nboot = number of Bootstrap samples, default=1000
# scaling = measurement scale ("nominal", "ordinal", "interval", "ratio"), default="nominal"

# output:
# observed agreement for the complete cases and for all cases with at least two ratings (obs.agr.k, obs.agr.alpha)
# point estimators: est.k, est.alpha
# confidence intervals: ci.asympt_k, ci.boot,k, ci.boot.alpha


#' @title
#' Calculate Fleiss' kappa and Krippendorff's alpha
#'
#' @description
#' This function was adapted from the paper _Measuring inter-rater reliability
#' for nominal data – which coefficients and confidence intervals are
#' appropriate?_ by Zapf et al. Their work compared the two reliability measures
#' and confidence intervals to see which provide the best statistical properties
#' for the assessment of inter-rater reliability in different situations. With
#' their paper they provided supplemental R code which was adapted to this
#' function. Most is kept intact, but I adjusted the output to return a `tibble`
#' instead of a list and text like they had done.
#'
#' Their conclusions: Fleiss' K and Krippendorff's alpha with bootstrap
#' confidence intervals are equally suitable for the analysis of reliability of
#' complete nominal data. The asymptotic confidence interval for Fleiss' K
#' should not be used. In the case of missing data or data or higher than
#' nominal order, Krippendorff’s alpha is recommended.
#'
#' @references
#' /url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4974794/}
#' Efron B. Six questions raised by the bootstrap. Exploring the limits of
#' bootstrap. Editors LePage R, Billard L. Technical Report No. 139. Division of
#' Biostatistics, Stanford University. Wiley & Sons, New York; 1992.
#'
#'
#' @param ratings_t A matirx or data frame or tibble of the ratings. Rows =
#'   individuals; columns = raters. Missing values coded by `NA`.
#' @param alpha_q Numeric; two-sided type one error, default = 0.05
#' @param nboot Integer; number of Bootstrap samples, defaul = 1000
#' @param scaling String; measurement scale ("nominal", "ordinal", "interval",
#'   "ratio"), default = "nominal"
#'
#' @importFrom tibble tibble
#' @importFrom stats na.omit
#' @importFrom stats quantile
#'
#' @return A tibble with the following columns
#' \item{measure}{Name of the measure}
#' \item{scale}{Measurement scale}
#' \item{N_subjects}{For kappa: number of subjects without missing values; For
#' alpha: number of subjects with two or more ratings.}
#' \item{n_raters}{Number of raters}
#' \item{k_categories}{Number of categories}
#' \item{obs_agr}{Observed agreement for the complete cases and for all cases
#' with at least two ratings}
#' \item{estimate}{Point estimates: Fleiss' kappa and Krippendorff's alpha}
#' \item{ci_asym_lower}{Lower asymptotic confidence interval}
#' \item{ci_asym_upper}{Upper asymptotic confidence interval}
#' \item{ci_boot_lower}{Lower bootstrap confidence interval}
#' \item{ci_boot_upper}{Upper bootstrap confidence interval}
#' @export
#'
#' @examples
#' ratings_t <- matrix(ncol = 3, nrow = 10,
#'                     c(5, 5, 5,
#'                       3, 5, 5,
#'                       1, 4, 4,
#'                       3, 3, 3,
#'                       4, 4, 5,
#'                       1, 3, 4,
#'                       3, 3, 3,
#'                       1, 1, 3,
#'                       2, 2, 5,
#'                       3, 3, 4),
#'                     byrow = TRUE)
#' k_alpha(ratings_t,
#'         alpha_q = 0.05,
#'         nboot = 1000,
#'         scaling = "nominal")
#' k_alpha(as.data.frame(ratings_t),
#'         alpha_q = 0.05,
#'         nboot = 1000,
#'         scaling = "nominal")
#' k_alpha(tibble::as_tibble(ratings_t),
#'         alpha_q = 0.05,
#'         nboot = 1000,
#'         scaling = "nominal")

k_alpha <- function(ratings_t,
                    alpha_q = 0.05,
                    nboot = 1000,
                    scaling = "nominal") {

  #### Helper functions to estimate stats --------------------------------

  ## Function for the estimation of Fleiss' K ----------------

  k_func <- function(N, n, k, ratings, categ) {
    # n_ij = number of raters who classed subject i in category j
    n_ij = matrix(ncol = k, nrow = N)
    step = 1
    for (j in categ) {
      for (i in 1:N) {
        n_ij[i, step] = sum(as.numeric(ratings[i, ] == j))
      }
      step = step + 1
    }
    # estimation of K_j
    p_j = apply(n_ij, 2, sum) / (N * n)
    q_j = 1 - p_j
    k_j = 1 - apply(n_ij * (n - n_ij), 2, sum) / (N * n * (n - 1) * p_j *
                                                    q_j)
    # estimation of the overall K
    k_t = sum(p_j * q_j * k_j) / sum(p_j * q_j)
    return(list(k_t, p_j, q_j))
  }

  ## Function for the estimation of Krippendorff's alpha ----------------

  alpha_func <- function(k, n, N, ratings, categ) {
    # conicidence matrix
    CM = matrix(ncol = k, nrow = k, 0)
    vn <- function(datavec)
      sum(!is.na(datavec))
    if (any(is.na(ratings)))
      mc = apply(ratings, 1, vn) - 1
    else
      mc = rep(n - 1, N)
    for (i in 1:N) {
      for (j in 1:(n - 1)) {
        for (jt in (j + 1):n) {
          if (!is.na(ratings[i, j]) && !is.na(ratings[i, jt])) {
            index1 = which(categ == ratings[i, j])
            index2 = which(categ == ratings[i, jt])
            CM[index1, index2] = CM[index1, index2] + (1 + (index1 == index2)) /
              mc[i]
            if (index1 != index2) {
              CM[index2, index1] = CM[index1, index2]
            }
          }
        }
      }
    }
    nmv <- sum(apply(CM, 2, sum))
    nc = apply(CM, 1, sum)
    ncnk = matrix(0, nrow = k, ncol = k)

    # matrix of expected disagreement
    D_e = matrix(0, ncol = k, nrow = k)
    for (C in 1:k) {
      for (Ct in 1:k) {
        if (C == Ct) {
          D_e[C, Ct] = nc[C] * (nc[Ct] - 1) / (nmv - 1)
        }
        if (C != Ct) {
          D_e[C, Ct] = nc[C] * nc[Ct] / (nmv - 1)
        }
        ncnk[C, Ct] = nc[C] * nc[Ct]
        ncnk[Ct, C] = ncnk[C, Ct]
      }
    }

    # matrix of metric differences
    diff2 = matrix(0, nrow = k, ncol = k)
    # nominal
    if (match(scaling[1], "nominal", 0)) {
      diff2 = matrix(1, ncol = k, nrow = k)
      diag(diff2) = 0
    }
    # ordinal
    if (match(scaling[1], "ordinal", 0)) {
      for (C in 1:k) {
        for (Ct in 1:k) {
          if (C != Ct) {
            tmp = nc[C:Ct]
            diff2[C, Ct] = (sum(tmp) - nc[C] / 2 - nc[Ct] / 2) ^ 2
            diff2[Ct, C] = diff2[C, Ct]
          }
        }
      }
    }
    # interval
    if (match(scaling[1], "interval", 0)) {
      for (C in 1:k) {
        for (Ct in 1:k) {
          if (C != Ct) {
            diff2[C, Ct] = (as.numeric(categ)[C] - as.numeric(categ)[Ct]) ^ 2
            diff2[Ct, C] = diff2[C, Ct]
          }
        }
      }
    }
    # ratio
    if (match(scaling[1], "ratio", 0)) {
      for (C in 1:k) {
        for (Ct in 1:k) {
          if (C != Ct) {
            diff2[C, Ct] = ((as.numeric(categ)[C] - as.numeric(categ)[Ct]) /
                              (as.numeric(categ)[C] + as.numeric(categ)[Ct])) ^
              2
            diff2[Ct, C] = diff2[C, Ct]
          }
        }
      }
    }
    # point estimator of Krippendorff's alpha
    tmp = diff2 * CM
    num = sum(tmp)
    tmp = diff2 * D_e
    den = sum(tmp)
    if (den > 0) {
      alpha_boot = 1 - num / den
    }
    if (den <= 0) {
      alpha_est = NA
    }
    return(alpha_boot)
  }


  #### Fleiss' K --------------------------------

  # check, if measurement scale is nominal
  if (match(scaling[1], "nominal", 0)) {
    # deleting all subjects with missing values
    ratings_c <- as.matrix(na.omit(ratings_t))
    # N = number of subjects, n = number of raters, k = number of categories
    N_c = nrow(ratings_c)

    v = function(dat) {
      min(dat) == max(dat)
    }
    agr_k = sum(apply(ratings_c, 1, v)) / N_c

    # TODO
    # check, if there are at least two individuals without missing values
    if (N_c < 2) {
      print(
        "There are less than two subjects withour missing values. Therefore, Fleiss' K cannot be                               calculated."
      )
    }

    if (N_c >= 2) {
      n_c = ncol(ratings_c)
      categ_c = levels(as.factor(ratings_c))
      k_c = length(categ_c)

      # point estimator of Fleiss` K
      k_ = k_func(N_c, n_c, k_c, ratings_c, categ_c)
      k_est = k_[[1]]
      p_j = k_[[2]]
      q_j = k_[[3]]

      ## asymptotic confidence interval ----------------

      # estimation of the standard error
      se_k = (sqrt(2) / (sum(p_j * q_j) * sqrt(N_c * n_c * (n_c - 1)))) *
        sqrt(sum(p_j * q_j) ^ 2 - sum(p_j * q_j * (q_j - p_j)))

      # asymptotic confidence interval for Fleiss' K
      CI_asymp_k = k_est + c(-1, 1) * qnorm(1 - alpha_q / 2) * se_k
    }
  }

  #### Krippendorff's alpha --------------------------------

  # deleting all subject with less than two ratings
  f <- function(x) sum(!is.na(x))

  # deleting all subjects with only one rating
  ratings = as.matrix(ratings_t[apply(ratings_t, 1, f) > 1, ])

  # N = number of subjects, n = number of raters, k = number of categories
  N_kr = nrow(ratings)

  v <- function(dat) {
    min(dat, na.rm = TRUE) == max(dat, na.rm = TRUE)
  }
  agr_alpha = sum(apply(ratings, 1, v)) / N_kr

  n_kr = ncol(ratings)
  categ = levels(as.factor(ratings))
  k_kr = length(categ)

  # point estimator of Krippendorff's alpha
  alpha_est = alpha_func(k_kr, n_kr, N_kr, ratings, categ)


  #### Bootstrap confidence intervals --------------------------------

  # K and alpha in each Bootstrap sample
  k_boot = 0
  alpha_boot = 0
  for (iboot in 1:nboot) {
    if (match(scaling[1], "nominal", 0)) {
      index.new = sample(seq(1, N_c, 1), N_c, replace = TRUE)
      ratings_b = ratings_c[index.new, ]
      n = ncol(ratings_b)
      categ = levels(as.factor(ratings_b))
      k = length(categ)
      k.b <- k_func(N_c, n, k, ratings_b, categ)[[1]]
      k_boot = c(k_boot, k.b)
    }
    f <-  function(x) sum(!is.na(x))
    # deleting all subjects with only one rating
    index.new = sample(seq(1, N_kr, 1), N_kr, replace = TRUE)
    ratings_b = ratings[index.new, ]
    n = ncol(ratings)
    categ = levels(as.factor(ratings))
    k = length(categ)
    alpha_b = alpha_func(k, n, N_kr, ratings_b, categ)
    alpha_boot = c(alpha_boot, alpha_b)
  }
  # confidence interval using the percentiles from the Bootstrap samples
  if (match(scaling[1], "nominal", 0)) {
    CI_boot_k = quantile(k_boot[-1],
                         probs = c(alpha_q / 2, 1 - alpha_q / 2),
                         na.rm = TRUE)
  }
  # confidence interval using the percentiles from the Bootstrap samples
  CI_boot_alpha = quantile(alpha_boot[-1],
                           probs = c(alpha_q / 2, 1 - alpha_q / 2),
                           na.rm = TRUE)


  #### Output --------------------------------

  ## For nominal data ----------------

  if (match(scaling[1], "nominal", 0)) {

    return(tibble::tibble(
      measure = c("Fleiss' kappa", "Krippendorff's alpha"),
      scale = c(scaling[1], scaling[1]),
      # for kappa: N (number of subjects without missing values)
      # alpha: N (number of subjects with two or more ratings)
      N_subjects = c(N_c, N_kr),
      n_raters = c(n_c, n_kr),
      k_categories = c(k_c, k_kr),
      obs_agr = c(agr_k, agr_alpha),
      estimate = c(k_est, alpha_est),
      ci_asym_lower = c(CI_asymp_k[[1]], NA),
      ci_asym_upper = c(CI_asymp_k[[2]], NA),
      ci_boot_lower = c(CI_boot_k[[1]], CI_boot_alpha[[1]]),
      ci_boot_upper = c(CI_boot_k[[2]], CI_boot_alpha[[2]])
      ))
  }

  ## For not nominal data ----------------

  if (!match(scaling[1], "nominal", 0)) {
    # Fleiss' K cannot be calculated, because it is only appropriate for nominal
    # data.

    tibble::tibble(
      measure = c("Fleiss' kappa", "Krippendorff's alpha"),
      scale = c(scaling[1], scaling[1]),
      # for kappa: N (number of subjects without missing values)
      # alpha: N (number of subjects with two or more ratings)
      N_subjects = c(NA, N_kr),
      n_raters = c(NA, n_kr),
      k_categories = c(NA, k_kr),
      obs_agr = c(NA, agr_alpha),
      estimate = c(NA, alpha_est),
      ci_asym_lower = c(NA, NA),
      ci_asym_upper = c(NA, NA),
      ci_boot_lower = c(NA, CI_boot_alpha[[1]]),
      ci_boot_upper = c(NA, CI_boot_alpha[[2]])
    )
  }

}
