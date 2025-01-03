#' Calculate Collinearity Diagnostics
#'
#' This function computes collinearity diagnostics, including variance inflation factors (VIF), tolerance, R-squared values, eigenvalues, condition indices, and more. It replicates functionality similar to what is described in the [Stata collinearity diagnostics page](http://www.philender.com/courses/categorical/notes2/collin.html).
#'
#' @param data A data frame containing the variables to analyze.
#' @param ... Variables to include in the analysis, specified without quotes.
#' @param method The method for calculating the correlation matrix. Default is `"pearson"`.
#' @param use How to handle missing values when calculating correlations. Default is `"complete.obs"`.
#' @param method_for_eigen Specifies the method for calculating eigenvalues and condition indices. Options are `"corr"` for the correlation matrix or `"sscp"` for the scaled sum of squares and cross-product matrix. Default is `"corr"`.
#' @param show_inv_cor_mat Logical. If `TRUE`, includes the inverse correlation matrix in the output. Default is `FALSE`.
#'
#' @return A list with the following components:
#' \item{table}{A tibble with the collinearity diagnostics for each variable. Includes VIF, tolerance, R-squared, eigenvalues, and condition indices.}
#' \item{summary}{A tibble summarizing the mean VIF, condition number, and determinant of the correlation matrix.}
#' \item{inv_cor_mat}{The inverse correlation matrix, if `show_inv_cor_mat = TRUE`.}
#'
#' @examples
#' # Example data
#' library(dplyr)
#' # Examples from Phil Ender
#' # http://www.philender.com/courses/categorical/notes2/collin.html
#'
#' hsbdemo <- read.csv("https://stats.idre.ucla.edu/stat/data/hsbdemo.csv")
#' dplyr::glimpse(hsbdemo)
#'
#' calc_collin_diag(data = hsbdemo,
#'                  female,
#'                  schtyp,
#'                  read,
#'                  write,
#'                  math,
#'                  science,
#'                  socst,
#'                  method_for_eigen = "corr",
#'                  method  = "pearson")
#'
#'
#' set.seed(123) # Ensure reproducibility
#'
#' n <- 100 # Number of rows
#'
#' lahigh <- tibble(
#'   id = 1000 + seq_len(n),
#'   gender = sample(c("male", "female"), n, replace = TRUE),
#'   ethnic = sample(c("hispanic", "filipino", "afr-amer", "asian", "white"), n, replace = TRUE),
#'   school = sample(1:2, n, replace = TRUE),
#'   algebra = sample(0:4, n, replace = TRUE),
#'   math = sample(0:4, n, replace = TRUE),
#'   eng95 = sample(0:4, n, replace = TRUE),
#'   eng94 = sample(0:4, n, replace = TRUE),
#'   mathnce = runif(n, 1, 100), # Continuous values between 1 and 100
#'   langnce = runif(n, 1, 100),
#'   mathpr = sample(1:100, n, replace = TRUE), # Integer percentiles
#'   langpr = sample(1:100, n, replace = TRUE),
#'   biling = sample(0:3, n, replace = TRUE),
#'   engprof = sample(0:4, n, replace = TRUE),
#'   daysatt = sample(40:90, n, replace = TRUE),
#'   daysabs = sample(0:35, n, replace = TRUE)
#' )
#'
#' dplyr::glimpse(lahigh)
#'
#' calc_collin_diag(data = lahigh,
#'                  mathnce,
#'                  langnce,
#'                  mathpr,
#'                  langpr,
#'                  method_for_eigen = "corr",
#'                  method  = "pearson")
#'
#' calc_collin_diag(data = lahigh,
#'                  mathnce,
#'                  langnce,
#'                  method_for_eigen = "corr",
#'                  method  = "pearson")
#'
#' @export
#'
#'





calc_collin_diag <- function(data,
                             ...,
                             method = "pearson",
                             use = "complete.obs",
                             method_for_eigen = "corr",
                             show_inv_cor_mat = FALSE) {

  # Fix no visible binding for global variable
  variable <- predictors <- form <- r_squared <- vif <- sqrt_vif <- tolerance <- eigenval <- rnk <- cond_index <- NULL

  # vars <- rlang::enquos(...)


  #### Make sure that all variables are numeric --------------------------------

  # data <- data %>%
  #   dplyr::select(!!! vars) %>%
  #   mutate_all(.tbl = .,
  #              .funs = list(~ as.numeric(.)))

  data <- data %>%
    dplyr::select(...) %>%
    mutate(dplyr::across(.cols = dplyr::everything(),
                         .fns = ~ make_numeric(.)))

  vars_names <- names(data)


  #### Rsqrd, vif, tolerance --------------------------------

  res <- tibble::tibble(
    variable = vars_names
  ) %>%
    mutate(predictors = purrr::map(.x = variable,
                                   .f = ~ vars_names[!vars_names %in% .x]),
           form = purrr::map2_chr(.x = variable,
                                  .y = predictors,
                                  .f = ~ paste0(.x, " ~ ", paste0(.y, collapse = " + "))),
           r_squared = purrr::map_dbl(.x = form,
                                      .f = ~ calc_r_squared(form = .x,
                                                            data = data)),
           vif = 1 / (1 - r_squared),
           sqrt_vif = sqrt(vif),
           tolerance = 1 / vif) %>%
    dplyr::select(variable,
                  vif,
                  sqrt_vif,
                  tolerance,
                  r_squared) %>%
    {.}


  cor_mat <- cor(data,
                 method = method,
                 use = use)


  #### Eigenvals, Conditional index --------------------------------

  ## deviation sscp (no intercept) ----------------
  # Eigenvalues and condition index computed from correlation matrix without a
  # constant.

  if (method_for_eigen == "corr") {

    svd_x <- svd(cor_mat)

    res2 <- tibble::tibble(
      variable = colnames(cor_mat),
      eigenval = eigen(cor_mat)$values,
      cond_index = sqrt(max(svd_x$d) / svd_x$d),
      rnk = rank(dplyr::desc(eigenval))
    ) %>%
      dplyr::select(variable, rnk, eigenval, cond_index)

    res  <- res  %>%
      dplyr::left_join(.,
                       res2,
                       by = "variable")

  }


  ## scaled raw sscp (w/ intercept) ----------------
  # By default the eigenvalues and condition index are computed on the scaled
  # raw score SSCP matrix with an intercept.

  if (method_for_eigen == "sscp") {

    sscp <- data %>%
      mutate(constant = 1) %>%
      as.matrix() %>%
      crossprod()

    diag_1 <- sscp * diag(ncol(sscp))
    diag_1 <- sqrt(diag_1)
    diag_1 <- syminv(diag_1)
    sscp <- diag_1 %*% sscp %*% diag_1

    svd_x <- svd(sscp)

    res2 <- tibble::tibble(
      variable = c(colnames(data), "Intercept"),
      eigenval = eigen(sscp)$values,
      cond_index = sqrt(max(svd_x$d) / svd_x$d),
      rnk = rank(dplyr::desc(eigenval))
    ) %>%
      dplyr::select(variable, rnk, eigenval, cond_index)

    res  <- res  %>%
      dplyr::bind_rows(.,
                       tibble::tibble(variable = "Intercept",
                                      vif = NA_real_,
                                      sqrt_vif = NA_real_,
                                      tolerance = NA_real_,
                                      r_squared = NA_real_)) %>%
      dplyr::left_join(.,
                       res2,
                       by = "variable")

  }


  #### Summary results --------------------------------

  summary_res <- tibble::tibble(
    info = c("Mean VIF",
             "Condition Number",
             "Determinant of Correlation Matrix"),
    rstls = c(mean(res$vif, na.rm = TRUE),
              max(res$cond_index),
              det(cor_mat))
  )


  results <- list(table = res,
                  summary = summary_res)


  #### Inverse correlation matrix --------------------------------

  if (show_inv_cor_mat == TRUE) {

    inv_cor_mat <- cor_mat %>%
      solve(.) %>%
      round(.,
            digits = 5)

    results <- list(table = res,
                    summary = summary_res,
                    inv_cor_mat = inv_cor_mat)

  }


  #### End of function --------------------------------

  return(results)

}


#' Calculate R-Squared Value
#'
#' This helper function computes the R-squared value for a given formula and dataset.
#'
#' @param form A formula specifying the regression model (e.g., `"y ~ x1 + x2"`).
#' @param data A data frame containing the variables used in the formula.
#'
#' @importFrom stats lm
#'
#' @return The R-squared value from the linear model.
#'
#' @examples
#' data <- mtcars
#' form <- "mpg ~ disp + hp + wt"
#' calc_r_squared(form, data)
#'
#' @export
calc_r_squared <- function(form, data) {


  # One way - old version
  # mod1 <- glm(as.formula(form),
  #             data = data,
  #             family = gaussian(link = "identity"))
  #
  # # rsq::rsq(mod1)
  # with(summary(mod1), 1 - deviance/null.deviance)

  # Another way
  # mod1 <- lm(as.formula(form),
  #            data = data)
  #
  # summary(mod1)$r.squared

  # Manual method
  # Fit the model
  model <- lm(as.formula(form), data = data)

  y <- purrr::pluck(model, "model", 1)

  # Predicted values
  y_pred <- predict(model)

  # Residual sum of squares (SSR)
  ssr <- sum((y - y_pred)^2)

  # Total sum of squares (SST)
  sst <- sum((y - mean(y))^2)

  # R-squared
  r_squared_manual <- 1 - (ssr / sst)

  return(r_squared_manual)

}


#' Symmetric Inverse
#'
#' Computes the inverse of a symmetric positive-definite matrix using its Cholesky decomposition.
#'
#' @param x A symmetric positive-definite matrix.
#'
#' @return The inverse of the input matrix.
#'
#' @examples
#' mat <- matrix(c(4, 2, 2, 3), ncol = 2)
#' syminv(mat)
#'
#' @export
syminv <- function(x) {

  # https://rdrr.io/cran/MNM/src/R/syminv.R
  # http://www.philender.com/courses/multivariate/notes/magic.html

  ch_x <- chol(x)
  chol2inv(ch_x)
}



#' Convert a Column to Numeric Codes
#'
#' This function converts a character or factor column into numeric codes, preserving numeric columns as-is. Character and factor variables are first converted to factors and then assigned numeric codes based on their levels.
#'
#' @param col A vector (column) to be converted. It can be a character, factor, or numeric vector.
#'
#' @return A numeric vector. If the input is a character or factor column, it is converted to numeric codes. Numeric columns are returned unchanged.
#'
#' @examples
#' # Example usage
#' char_col <- c("a", "b", "a", "c")
#' factor_col <- factor(c("low", "medium", "high"))
#' numeric_col <- c(1.2, 3.4, 5.6)
#'
#' # Convert columns to numeric codes
#' make_numeric(char_col)   # Returns numeric codes for characters
#' make_numeric(factor_col) # Returns numeric codes for factors
#' make_numeric(numeric_col) # Returns numeric values unchanged
#'
#' @export
make_numeric <- function(col) {
  if (is.character(col) || is.factor(col)) {
    as.numeric(as.factor(col))  # Convert characters or factors to numeric codes
  } else {
    col  # Leave numeric columns as is
  }
}





# lahigh <- tibble::tribble(
#   ~id,  ~gender,    ~ethnic, ~school, ~algebra, ~math, ~eng95, ~eng94, ~mathnce, ~langnce, ~mathpr, ~langpr, ~biling, ~engprof, ~daysatt, ~daysabs,
#   1001L,   "male", "hispanic",      1L,       3L,    0L,     2L,     2L, 56.98883, 42.45086,     63L,     36L,      2L,       2L,      73L,       4L,
#   1002L,   "male", "hispanic",      1L,       1L,    2L,     2L,     4L, 37.09416, 46.82059,     27L,     44L,      2L,       3L,      73L,       4L,
#   1003L, "female", "hispanic",      1L,       3L,    3L,     4L,     3L, 32.27546, 43.56657,     20L,     38L,      2L,       3L,      76L,       2L,
#   1004L, "female", "hispanic",      1L,       1L,    4L,     3L,     4L, 29.05672, 43.56657,     16L,     38L,      2L,       2L,      74L,       3L,
#   1005L, "female", "hispanic",      1L,       0L,    1L,     0L,     2L, 6.748048, 27.24847,      2L,     14L,      3L,       1L,      73L,       3L,
#   1006L, "female", "hispanic",      1L,       4L,    1L,     3L,     4L, 61.65428, 48.41482,     71L,     47L,      0L,       1L,      62L,      13L,
#   1007L, "female", "hispanic",      1L,       0L,    1L,     1L,     3L, 56.98883, 40.73543,     63L,     33L,      2L,       2L,      66L,      11L,
#   1008L,   "male", "hispanic",      1L,       0L,    0L,     0L,     0L, 10.39049, 15.35938,      3L,      5L,      2L,       1L,      72L,       7L,
#   1009L,   "male", "hispanic",      1L,       2L,    2L,     3L,     2L, 50.52795, 52.11514,     51L,     54L,      2L,       1L,      63L,      10L,
#   1010L,   "male", "filipino",      1L,       0L,    2L,     0L,     1L, 49.47205, 42.45086,     49L,     36L,      0L,       0L,      68L,       9L,
#   1011L, "female", "filipino",      1L,       1L,    0L,     1L,     1L, 39.55739, 36.45115,     31L,     26L,      0L,       0L,      72L,       4L,
#   1012L,   "male", "hispanic",      1L,       4L,    4L,     3L,     4L, 33.73761, 13.13055,     22L,      4L,      2L,       2L,      74L,       5L,
#   1013L, "female", "hispanic",      1L,       3L,    4L,     3L,     3L, 62.90584, 62.27464,     73L,     72L,      2L,       2L,      72L,       5L,
#   1014L, "female", "afr-amer",      1L,       0L,    2L,     2L,     3L, 65.56011, 44.66451,     77L,     40L,      0L,       0L,      74L,       3L,
#   1015L,   "male", "filipino",      1L,       0L,    3L,     1L,     1L, 23.01052, 25.25478,     10L,     12L,      1L,       0L,      76L,       1L,
#   1016L,   "male", "hispanic",      1L,       4L,    3L,     4L,     2L, 75.83068, 61.04388,     89L,     70L,      2L,       2L,      76L,       0L,
#   1017L, "female", "hispanic",      1L,       2L,    2L,     4L,     4L, 41.31353, 49.47205,     34L,     49L,      2L,       1L,      75L,       1L,
#   1018L, "female", "hispanic",      1L,       0L,    2L,     4L,     3L, 41.88515, 65.56011,     35L,     77L,      2L,       1L,      74L,       0L,
#   1019L,   "male", "afr-amer",      1L,       1L,    2L,     2L,     1L, 65.56011, 46.82059,     77L,     44L,      1L,       0L,      75L,       2L,
#   1020L,   "male", "hispanic",      1L,       3L,    0L,     0L,     0L, 13.13055, 6.748048,      4L,      2L,      3L,       2L,      55L,      24L,
#   1021L, "female", "hispanic",      1L,       0L,    1L,     3L,     2L, 33.01677, 42.45086,     21L,     36L,      2L,       3L,      75L,       2L,
#   1022L,   "male", "hispanic",      1L,       0L,    2L,     2L,     2L, 55.88246, 64.87473,     61L,     76L,      2L,       1L,      76L,       0L,
#   1023L,   "male", "hispanic",      1L,       3L,    4L,     3L,     3L,  45.2079, 55.33549,     41L,     60L,      2L,       1L,      76L,       1L,
#   1024L,   "male", "hispanic",      1L,       2L,    2L,     2L,     2L, 56.98883, 44.66451,     63L,     40L,      2L,       2L,      76L,       0L,
#   1025L, "female", "hispanic",      1L,       3L,    4L,     4L,     3L,  31.5115, 38.34572,     19L,     29L,      2L,       1L,      71L,       8L,
#   1026L,   "male", "afr-amer",      1L,       0L,    2L,     3L,     3L, 52.64643,       50,     55L,     50L,      0L,       0L,      67L,       3L,
#   1027L,   "male", "hispanic",      1L,       0L,    1L,     0L,     2L, 17.25647, 6.748048,      6L,      2L,      0L,       0L,      70L,       7L,
#   1028L, "female", "hispanic",      1L,       0L,    2L,     1L,     2L, 33.01677, 40.15026,     21L,     32L,      2L,       2L,      76L,       0L,
#   1029L,   "male", "hispanic",      1L,       3L,    1L,     2L,     2L, 61.04388, 57.54914,     70L,     64L,      1L,       1L,      74L,       2L,
#   1030L,   "male", "afr-amer",      1L,       1L,    4L,     2L,     3L, 66.98323, 71.82729,     79L,     85L,      0L,       0L,      77L,       0L,
#   1031L,   "male", "hispanic",      1L,       1L,    1L,     1L,     1L, 1.007114,  45.2079,      1L,     41L,      1L,       4L,      77L,       0L,
#   1032L, "female", "hispanic",      1L,       1L,    4L,     2L,     3L, 38.34572, 35.12527,     29L,     24L,      3L,       1L,      76L,       1L,
#   1033L,   "male", "hispanic",      1L,       1L,    3L,     3L,     1L, 44.66451, 46.82059,     40L,     44L,      2L,       2L,      32L,       3L,
#   1034L,   "male",    "asian",      1L,       2L,    4L,     3L,     2L, 44.11754, 46.82059,     39L,     44L,      3L,       2L,      77L,       0L,
#   1035L,   "male", "hispanic",      1L,       2L,    3L,     2L,     0L, 59.84974, 46.28556,     68L,     43L,      3L,       2L,      77L,       0L,
#   1036L, "female", "hispanic",      1L,       0L,    4L,     0L,     3L, 32.27546, 47.35357,     20L,     45L,      3L,       1L,      49L,      28L,
#   1037L,   "male", "hispanic",      1L,       0L,    0L,     2L,     1L, 23.01052, 49.47205,     10L,     49L,      1L,       0L,      71L,       8L,
#   1038L,   "male", "hispanic",      1L,       2L,    2L,     3L,     3L, 70.94328, 61.04388,     84L,     70L,      2L,       4L,      72L,       5L,
#   1039L,   "male", "afr-amer",      1L,       0L,    2L,     1L,     0L, 1.007114, 1.007114,      1L,      1L,      0L,       0L,      75L,       2L,
#   1040L, "female", "hispanic",      1L,       0L,    0L,     1L,     0L, 41.88515, 52.11514,     35L,     54L,      3L,       2L,      46L,      27L,
#   1041L, "female", "hispanic",      1L,       3L,    3L,     3L,     4L, 40.15026, 35.12527,     32L,     24L,      2L,       2L,      72L,       5L,
#   1042L, "female", "hispanic",      1L,       0L,    1L,     0L,     2L, 41.31353, 38.34572,     34L,     29L,      3L,       2L,      59L,      18L,
#   1043L, "female", "hispanic",      1L,       0L,    4L,     2L,     3L, 44.66451, 58.68647,     40L,     66L,      3L,       0L,      57L,      19L,
#   1044L,   "male", "hispanic",      1L,       1L,    2L,     1L,     1L, 38.34572, 42.45086,     29L,     36L,      2L,       2L,      67L,       9L,
#   1045L,   "male", "hispanic",      1L,       2L,    3L,     0L,     0L, 32.27546, 1.007114,     20L,      1L,      1L,       2L,      68L,       9L,
#   1046L, "female", "hispanic",      1L,       3L,    4L,     3L,     1L, 37.09416, 32.27546,     27L,     20L,      0L,       1L,      75L,       4L,
#   1047L,   "male", "hispanic",      1L,       2L,    2L,     2L,     0L, 63.54885, 57.54914,     74L,     64L,      2L,       1L,      75L,       2L,
#   1048L,   "male", "hispanic",      1L,       1L,    2L,     2L,     2L, 43.56657, 41.31353,     38L,     34L,      1L,       1L,      68L,       3L,
#   1049L,   "male", "hispanic",      1L,       0L,    0L,     2L,     0L, 33.01677, 24.16932,     21L,     11L,      0L,       1L,      68L,       9L,
#   1050L, "female",    "asian",      1L,       0L,    0L,     0L,     3L, 68.48849, 59.26457,     81L,     67L,      2L,       1L,      56L,      20L,
#   1051L,   "male", "hispanic",      1L,       0L,    3L,     0L,     2L, 29.05672,  21.7637,     16L,      9L,      1L,       0L,      65L,       6L,
#   1052L,   "male",    "asian",      1L,       4L,    2L,     4L,     2L,  54.7921,  54.7921,     59L,     59L,      0L,       1L,      76L,       0L,
#   1053L,   "male", "hispanic",      1L,       0L,    0L,     0L,     2L, 48.94376, 51.58518,     48L,     53L,      2L,       1L,      50L,      27L,
#   1054L, "female", "hispanic",      1L,       2L,    4L,     2L,     3L, 52.64643, 50.52795,     55L,     51L,      2L,       1L,      65L,      12L,
#   1055L, "female", "hispanic",      1L,       0L,    0L,     0L,     0L, 13.13055, 32.27546,      4L,     20L,      3L,       1L,      43L,      34L,
#   1056L,   "male",    "asian",      1L,       3L,    0L,     2L,     0L, 53.71444, 41.88515,     57L,     35L,      1L,       4L,      76L,       1L,
#   1057L,   "male", "afr-amer",      1L,       1L,    0L,     1L,     1L, 15.35938, 32.27546,      5L,     20L,      0L,       0L,      52L,      25L,
#   1058L,   "male", "afr-amer",      1L,       0L,    2L,     0L,     0L, 10.39049, 17.25647,      3L,      6L,      0L,       0L,      71L,       5L,
#   1059L, "female", "hispanic",      1L,       1L,    2L,     2L,     3L, 34.43988, 38.34572,     23L,     29L,      3L,       3L,      74L,       3L,
#   1060L, "female", "hispanic",      1L,       3L,    2L,     4L,     2L, 41.88515, 42.45086,     35L,     36L,      1L,       1L,      77L,       2L,
#   1061L,   "male", "hispanic",      1L,       0L,    0L,     1L,     1L, 40.73543, 55.33549,     33L,     60L,      3L,       2L,      76L,       1L,
#   1062L, "female", "hispanic",      1L,       1L,    1L,     2L,     1L, 37.09416, 52.64643,     27L,     55L,      2L,       2L,      70L,       7L,
#   1063L,   "male", "hispanic",      1L,       1L,    2L,     3L,     3L,  26.2782,  26.2782,     13L,     13L,      1L,       1L,      73L,       4L,
#   1064L,   "male", "hispanic",      1L,       2L,    0L,     3L,     1L, 47.88486, 36.45115,     46L,     26L,      3L,       1L,      71L,       8L,
#   1065L, "female", "hispanic",      1L,       0L,    0L,     1L,     0L, 38.95612, 55.88246,     30L,     61L,      0L,       0L,      71L,       6L,
#   1066L,   "male",    "asian",      1L,       0L,    0L,     0L,     1L, 40.15026, 47.35357,     32L,     45L,      0L,       0L,      61L,      16L,
#   1067L, "female", "afr-amer",      1L,       0L,    1L,     3L,     2L, 36.45115, 40.15026,     26L,     32L,      0L,       0L,      74L,       3L,
#   1068L, "female", "hispanic",      1L,       0L,    2L,     1L,     2L, 27.24847, 39.55739,     14L,     31L,      1L,       2L,      71L,       4L,
#   1069L, "female", "filipino",      1L,       1L,    3L,     3L,     3L, 39.55739, 43.56657,     31L,     38L,      1L,       0L,      73L,       3L,
#   1070L, "female", "hispanic",      1L,       0L,    2L,     1L,     1L, 48.94376, 36.45115,     48L,     26L,      3L,       3L,      72L,       5L,
#   1071L,   "male", "hispanic",      1L,       2L,    3L,     3L,     2L, 62.27464, 46.28556,     72L,     43L,      1L,       0L,      73L,       0L,
#   1072L,   "male", "hispanic",      1L,       2L,    3L,     2L,     3L, 66.98323, 59.84974,     79L,     68L,      0L,       1L,      67L,       9L,
#   1073L,   "male", "filipino",      1L,       2L,    2L,     3L,     3L, 28.17271, 59.84974,     15L,     68L,      3L,       0L,      77L,       0L,
#   1074L,   "male", "hispanic",      1L,       0L,    0L,     0L,     0L, 35.12527, 37.09416,     24L,     27L,      1L,       1L,      69L,       8L,
#   1075L, "female", "filipino",      1L,       3L,    4L,     4L,     3L, 52.64643, 70.09472,     55L,     83L,      0L,       0L,      77L,       0L,
#   1076L, "female", "afr-amer",      1L,       0L,    3L,     3L,     3L,  26.2782, 35.12527,     13L,     24L,      0L,       0L,      65L,      11L,
#   1077L,   "male", "hispanic",      1L,       3L,    4L,     1L,     3L, 41.31353, 42.45086,     34L,     36L,      0L,       1L,      75L,       4L,
#   1078L,   "male", "hispanic",      1L,       2L,    1L,     2L,     2L, 59.26457, 71.82729,     67L,     85L,      1L,       1L,      75L,       2L,
#   1079L,   "male", "hispanic",      1L,       0L,    0L,     0L,     0L, 1.007114, 17.25647,      1L,      6L,      3L,       1L,      35L,      35L,
#   1080L, "female", "hispanic",      1L,       0L,    0L,     0L,     2L, 33.01677, 41.88515,     21L,     35L,      3L,       0L,      54L,      23L,
#   1081L, "female", "hispanic",      1L,       0L,    4L,     0L,     3L, 40.73543, 42.45086,     33L,     36L,      3L,       2L,      64L,      13L,
#   1082L,   "male", "hispanic",      1L,       2L,    4L,     2L,     4L, 66.26239, 64.87473,     78L,     76L,      0L,       1L,      71L,       6L,
#   1083L, "female", "afr-amer",      1L,       4L,    2L,     3L,     2L, 70.94328, 45.74812,     84L,     42L,      0L,       0L,      79L,       0L,
#   1084L,   "male", "hispanic",      1L,       2L,    0L,     3L,     1L,  54.7921, 60.44261,     59L,     69L,      2L,       2L,      71L,       6L,
#   1085L,   "male", "hispanic",      1L,       3L,    1L,     2L,     2L, 64.20476, 50.52795,     75L,     51L,      2L,       1L,      77L,       0L,
#   1086L,   "male", "afr-amer",      1L,       1L,    2L,     2L,     1L, 33.73761, 40.15026,     22L,     32L,      0L,       0L,      66L,       8L,
#   1087L, "female", "hispanic",      1L,       0L,    3L,     1L,     2L, 49.47205, 44.66451,     49L,     40L,      0L,       0L,      66L,      11L,
#   1088L, "female", "hispanic",      1L,       0L,    3L,     4L,     2L,  26.2782, 73.72179,     13L,     87L,      2L,       1L,      66L,      11L,
#   1089L,   "male", "filipino",      1L,       4L,    3L,     4L,     3L, 70.94328, 54.25188,     84L,     58L,      1L,       3L,      73L,       4L,
#   1090L,   "male", "hispanic",      1L,       0L,    0L,     0L,     1L, 37.09416, 41.31353,     27L,     34L,      2L,       2L,      71L,       6L,
#   1091L, "female", "hispanic",      1L,       1L,    4L,     2L,     4L, 37.09416, 49.47205,     27L,     49L,      0L,       3L,      54L,      23L,
#   1092L, "female", "hispanic",      1L,       3L,    2L,     2L,     2L, 61.04388, 69.27759,     70L,     82L,      1L,       1L,      72L,       5L,
#   1093L,   "male", "hispanic",      1L,       1L,    3L,     4L,     2L, 86.86945, 86.86945,     96L,     96L,      2L,       2L,      72L,       5L,
#   1094L, "female", "afr-amer",      1L,       0L,    0L,     0L,     0L, 61.04388, 64.20476,     70L,     75L,      0L,       0L,      51L,      26L,
#   1095L, "female", "hispanic",      1L,       0L,    2L,     3L,     4L, 23.01052, 6.748048,     10L,      2L,      3L,       3L,      69L,       7L,
#   1096L,   "male", "hispanic",      1L,       0L,    1L,     0L,     2L, 27.24847, 35.12527,     14L,     24L,      3L,       1L,      76L,       1L,
#   1097L,   "male", "hispanic",      1L,       0L,    1L,     3L,     2L, 32.27546, 46.82059,     20L,     44L,      2L,       2L,      68L,       9L,
#   1098L,   "male", "hispanic",      1L,       0L,    0L,     0L,     0L, 24.16932, 17.25647,     11L,      6L,      2L,       2L,      66L,      11L,
#   1099L,   "male", "hispanic",      1L,       0L,    2L,     0L,     0L, 45.74812,       50,     42L,     50L,      2L,       1L,      60L,      18L,
#   1100L,   "male", "hispanic",      1L,       0L,    1L,     0L,     0L, 6.748048, 13.13055,      2L,      4L,      3L,       2L,      56L,      12L,
#   1101L,   "male", "afr-amer",      1L,       0L,    0L,     0L,     0L, 35.12527, 27.24847,     24L,     14L,      0L,       0L,      73L,       3L,
#   1102L,   "male",    "asian",      1L,       0L,    3L,     1L,     3L, 43.01117, 28.17271,     37L,     15L,      3L,       0L,      69L,       0L,
#   1103L,   "male", "hispanic",      1L,       0L,    0L,     2L,     0L,  26.2782, 13.13055,     13L,      4L,      3L,       1L,      73L,       4L,
#   1104L,   "male", "hispanic",      1L,       2L,    1L,     1L,     1L, 10.39049, 29.05672,      3L,     16L,      3L,       0L,      68L,      10L,
#   1105L,   "male", "afr-amer",      1L,       1L,    3L,     2L,     4L, 61.04388, 66.26239,     70L,     78L,      0L,       0L,      61L,      16L,
#   1106L, "female", "hispanic",      1L,       1L,    1L,     2L,     3L, 37.09416, 51.58518,     27L,     53L,      2L,       2L,      74L,       1L,
#   1107L, "female", "afr-amer",      1L,       0L,    2L,     4L,     1L, 1.007114, 1.007114,      1L,      1L,      0L,       0L,      70L,       9L,
#   1108L, "female",    "asian",      1L,       1L,    4L,     3L,     3L, 55.88246,  45.2079,     61L,     41L,      2L,       0L,      74L,       3L,
#   1109L,   "male", "hispanic",      1L,       3L,    3L,     3L,     3L, 46.82059, 38.34572,     44L,     29L,      3L,       3L,      77L,       0L,
#   1110L,   "male", "hispanic",      1L,       0L,    0L,     0L,     0L, 41.88515, 56.98883,     35L,     63L,      2L,       3L,      68L,       9L,
#   1111L,   "male",    "asian",      1L,       3L,    0L,     2L,     2L, 66.26239, 35.79525,     78L,     25L,      3L,       0L,      65L,      14L,
#   1112L, "female", "hispanic",      1L,       0L,    3L,     2L,     2L, 10.39049, 38.95612,      3L,     30L,      2L,       2L,      70L,       7L,
#   1113L,   "male", "hispanic",      1L,       0L,    1L,     0L,     1L, 33.01677, 40.15026,     21L,     32L,      3L,       4L,      74L,       3L,
#   1114L,   "male", "hispanic",      1L,       0L,    0L,     3L,     4L, 36.45115, 20.40919,     26L,      8L,      3L,       3L,      67L,      10L,
#   1115L,   "male", "hispanic",      1L,       0L,    3L,     2L,     2L, 46.82059, 65.56011,     44L,     77L,      2L,       1L,      64L,      12L,
#   1116L, "female", "hispanic",      1L,       2L,    1L,     2L,     0L, 48.41482, 34.43988,     47L,     23L,      1L,       0L,      71L,       6L,
#   1117L, "female", "hispanic",      1L,       0L,    0L,     1L,     2L, 41.88515, 41.88515,     35L,     35L,      2L,       0L,      42L,      35L,
#   1118L, "female", "afr-amer",      1L,       3L,    3L,     4L,     3L, 20.40919, 25.25478,      8L,     12L,      0L,       0L,      64L,      13L,
#   1119L, "female", "hispanic",      1L,       3L,    1L,     4L,     3L, 73.72179, 74.74522,     87L,     88L,      2L,       1L,      74L,       3L,
#   1120L,   "male", "hispanic",      1L,       0L,    2L,     3L,     2L, 10.39049, 40.15026,      3L,     32L,      3L,       2L,      65L,      10L,
#   1121L,   "male", "hispanic",      1L,       0L,    0L,     1L,     2L, 17.25647, 20.40919,      6L,      8L,      3L,       3L,      71L,       6L,
#   1122L,   "male", "afr-amer",      1L,       4L,    3L,     4L,     3L, 75.83068, 64.87473,     89L,     76L,      0L,       0L,      76L,       0L,
#   1123L,   "male", "hispanic",      1L,       1L,    1L,     1L,     0L,  26.2782, 23.01052,     13L,     10L,      3L,       1L,      75L,       2L,
#   1124L,   "male", "hispanic",      1L,       0L,    3L,     3L,     4L, 70.94328, 82.74353,     84L,     94L,      2L,       1L,      71L,       6L,
#   1125L,   "male", "hispanic",      1L,       4L,    4L,     2L,     2L, 76.98948, 68.48849,     90L,     81L,      1L,       0L,      72L,       5L,
#   1126L, "female", "hispanic",      1L,       1L,    4L,     3L,     4L, 61.65428, 58.11485,     71L,     65L,      2L,       2L,      62L,      13L,
#   1127L, "female", "afr-amer",      1L,       0L,    0L,     2L,     1L, 28.17271, 6.748048,     15L,      2L,      0L,       0L,      73L,       4L,
#   1128L, "female", "hispanic",      1L,       0L,    2L,     0L,     2L,  45.2079, 48.94376,     41L,     48L,      2L,       1L,      72L,       5L,
#   1129L, "female",    "asian",      1L,       4L,    3L,     4L,     3L, 56.98883, 65.56011,     63L,     77L,      0L,       0L,      74L,       3L,
#   1130L, "female", "hispanic",      1L,       0L,    1L,     0L,     1L, 41.31353, 48.94376,     34L,     48L,      2L,       2L,      44L,      30L,
#   1131L, "female", "hispanic",      1L,       0L,    1L,     1L,     1L, 47.35357, 46.82059,     45L,     44L,      0L,       2L,      61L,      16L,
#   1132L, "female", "hispanic",      1L,       1L,    0L,     0L,     1L, 38.34572, 43.01117,     29L,     37L,      0L,       2L,      62L,      15L,
#   1133L, "female", "hispanic",      1L,       1L,    2L,     1L,     0L, 10.39049, 6.748048,      3L,      2L,      3L,       2L,      65L,      12L,
#   1134L,   "male", "hispanic",      1L,       3L,    0L,     4L,     1L, 41.88515, 36.45115,     35L,     26L,      2L,       4L,      78L,       1L,
#   1135L,   "male", "afr-amer",      1L,       1L,    2L,     3L,     2L, 40.15026, 38.95612,     32L,     30L,      0L,       0L,      72L,       1L,
#   1136L,   "male", "hispanic",      1L,       0L,    3L,     2L,     2L, 33.73761, 35.79525,     22L,     25L,      0L,       0L,      70L,       7L,
#   1137L, "female", "hispanic",      1L,       4L,    1L,     4L,     4L, 68.48849, 52.64643,     81L,     55L,      1L,       2L,      74L,       1L,
#   1138L, "female",    "asian",      1L,       0L,    2L,     0L,     3L, 98.99289, 64.20476,     99L,     75L,      2L,       2L,      13L,      45L,
#   1139L, "female", "hispanic",      1L,       4L,    2L,     3L,     4L, 62.90584, 43.01117,     73L,     37L,      2L,       3L,      69L,      10L,
#   1140L,   "male",    "asian",      1L,       3L,    3L,     3L,     3L, 60.44261, 36.45115,     69L,     26L,      2L,       4L,      74L,       3L,
#   1141L, "female", "afr-amer",      1L,       1L,    0L,     2L,     1L, 37.09416, 10.39049,     27L,      3L,      0L,       0L,      52L,      27L,
#   1142L, "female", "hispanic",      1L,       2L,    1L,     2L,     2L,  26.2782, 33.01677,     13L,     21L,      2L,       2L,      74L,       2L,
#   1143L, "female", "hispanic",      1L,       0L,    0L,     1L,     1L, 64.87473, 53.71444,     76L,     57L,      2L,       1L,      58L,      13L,
#   1144L, "female", "afr-amer",      1L,       0L,    2L,     3L,     2L, 18.91984, 29.05672,      7L,     16L,      0L,       0L,      70L,       2L,
#   1145L,   "male", "hispanic",      1L,       0L,    2L,     0L,     2L,  54.7921, 47.88486,     59L,     46L,      2L,       1L,      74L,       5L,
#   1146L, "female", "hispanic",      1L,       0L,    2L,     2L,     4L, 38.34572, 38.95612,     29L,     30L,      2L,       1L,      64L,       5L,
#   1147L, "female", "hispanic",      1L,       0L,    4L,     1L,     4L, 53.71444, 52.11514,     57L,     54L,      2L,       2L,      69L,       4L,
#   1148L, "female", "hispanic",      1L,       3L,    0L,     4L,     3L, 46.82059, 51.58518,     44L,     53L,      1L,       3L,      76L,       3L,
#   1149L, "female", "hispanic",      1L,       0L,    3L,     3L,     3L, 51.58518, 71.82729,     53L,     85L,      1L,       1L,      44L,      20L,
#   1150L, "female", "hispanic",      1L,       0L,    1L,     1L,     2L, 34.43988, 40.15026,     23L,     32L,      3L,       1L,      63L,      12L,
#   1151L, "female", "afr-amer",      1L,       2L,    0L,     4L,     0L, 38.34572, 44.66451,     29L,     40L,      0L,       0L,      48L,      31L,
#   1152L,   "male",    "asian",      1L,       0L,    2L,     2L,     2L, 44.66451, 40.73543,     40L,     33L,      3L,       0L,      70L,       6L,
#   1153L,   "male", "hispanic",      1L,       0L,    4L,     2L,     0L, 49.47205,  31.5115,     49L,     19L,      2L,       3L,      63L,      14L,
#   1154L, "female", "afr-amer",      1L,       0L,    3L,     0L,     3L, 43.56657, 46.82059,     38L,     44L,      0L,       0L,      56L,      13L,
#   1155L,   "male", "afr-amer",      1L,       0L,    2L,     3L,     4L, 39.55739, 49.47205,     31L,     49L,      0L,       0L,      71L,       6L,
#   1156L, "female", "afr-amer",      1L,       0L,    0L,     0L,     0L, 33.01677, 47.35357,     21L,     45L,      0L,       0L,      65L,      12L,
#   1157L,   "male", "afr-amer",      1L,       0L,    2L,     0L,     0L, 20.40919, 29.05672,      8L,     16L,      0L,       0L,      65L,      12L,
#   1158L,   "male", "hispanic",      1L,       3L,    0L,     2L,     3L, 43.01117, 34.43988,     37L,     23L,      3L,       3L,      77L,       0L,
#   1159L,   "male", "hispanic",      1L,       3L,    4L,     4L,     3L, 51.58518, 43.56657,     53L,     38L,      2L,       1L,      78L,       1L,
#   2001L, "female",    "white",      2L,       0L,    1L,     0L,     3L, 39.55739, 50.52795,     31L,     51L,      0L,       0L,      82L,       4L,
#   2002L,   "male",    "white",      2L,       2L,    1L,     2L,     1L, 53.71444, 1.007114,     57L,      1L,      3L,       0L,      86L,       0L,
#   2003L,   "male", "filipino",      2L,       1L,    2L,     2L,     1L, 53.71444, 38.95612,     57L,     30L,      0L,       0L,      86L,       0L,
#   2004L,   "male",    "white",      2L,       1L,    2L,     2L,     3L,  54.7921, 64.20476,     59L,     75L,      1L,       0L,      84L,       2L,
#   2005L, "female",    "white",      2L,       1L,    2L,     1L,     2L, 38.34572,  54.7921,     29L,     59L,      0L,       0L,      85L,       1L,
#   2006L, "female",    "asian",      2L,       2L,    3L,     4L,     3L,  45.2079, 71.82729,     41L,     85L,      1L,       0L,      84L,       2L,
#   2007L,   "male",    "white",      2L,       2L,    3L,     3L,     2L, 61.65428, 58.68647,     71L,     66L,      0L,       0L,      86L,       0L,
#   2008L, "female",    "white",      2L,       1L,    2L,     2L,     2L,  54.7921,       50,     59L,     50L,      3L,       0L,      86L,       0L,
#   2009L,   "male",    "white",      2L,       1L,    3L,     2L,     4L, 61.65428,  54.7921,     71L,     59L,      0L,       0L,      86L,       0L,
#   2010L, "female",    "white",      2L,       1L,    4L,     3L,     2L, 61.04388, 58.68647,     70L,     66L,      0L,       0L,      79L,       7L,
#   2011L,   "male",    "white",      2L,       3L,    3L,     3L,     1L, 65.56011, 66.26239,     77L,     78L,      1L,       0L,      84L,       2L,
#   2012L, "female",    "asian",      2L,       1L,    1L,     2L,     3L, 61.04388, 71.82729,     70L,     85L,      1L,       0L,      76L,       9L,
#   2013L, "female",    "white",      2L,       1L,    4L,     3L,     3L, 73.72179, 55.33549,     87L,     60L,      2L,       0L,      80L,       6L,
#   2014L,   "male",    "white",      2L,       0L,    3L,     4L,     0L,  54.7921, 51.58518,     59L,     53L,      0L,       0L,      82L,       4L,
#   2015L,   "male",    "white",      2L,       1L,    3L,     2L,     2L, 58.11485, 58.11485,     65L,     65L,      0L,       0L,      85L,       1L,
#   2016L, "female", "hispanic",      2L,       1L,    3L,     2L,     3L, 37.72536, 46.82059,     28L,     44L,      2L,       3L,      79L,       7L,
#   2017L,   "male",    "white",      2L,       3L,    3L,     3L,     3L, 65.56011, 70.09472,     77L,     83L,      1L,       0L,      86L,       0L,
#   2018L,   "male",    "white",      2L,       3L,    2L,     3L,     3L, 58.11485, 68.48849,     65L,     81L,      1L,       0L,      86L,       0L,
#   2019L, "female",    "white",      2L,       2L,    2L,     4L,     4L, 62.90584, 71.82729,     73L,     85L,      0L,       0L,      82L,       4L,
#   2020L,   "male", "hispanic",      2L,       3L,    2L,     3L,     2L, 98.99289, 55.33549,     99L,     60L,      0L,       0L,      82L,       2L,
#   2021L, "female",    "white",      2L,       3L,    3L,     4L,     3L, 53.71444, 66.98323,     57L,     79L,      2L,       2L,      86L,       0L,
#   2022L, "female",    "white",      2L,       3L,    1L,     3L,     2L, 65.56011,       50,     77L,     50L,      0L,       0L,      82L,       4L,
#   2023L, "female", "hispanic",      2L,       3L,    2L,     4L,     2L, 53.71444, 65.56011,     57L,     77L,      0L,       0L,      84L,       2L,
#   2024L, "female", "afr-amer",      2L,       0L,    0L,     0L,     2L, 48.41482, 29.05672,     47L,     16L,      0L,       0L,      67L,      18L,
#   2025L, "female",    "white",      2L,       0L,    3L,     3L,     3L, 35.12527, 52.11514,     24L,     54L,      2L,       0L,      85L,       1L,
#   2026L,   "male",    "white",      2L,       2L,    2L,     3L,     3L, 70.94328, 70.94328,     84L,     84L,      0L,       0L,      86L,       0L,
#   2027L, "female",    "white",      2L,       2L,    3L,     3L,     3L, 62.90584, 65.56011,     73L,     77L,      2L,       0L,      85L,       1L,
#   2028L,   "male",    "white",      2L,       1L,    1L,     2L,     1L, 68.48849, 58.68647,     81L,     66L,      0L,       0L,      70L,      16L,
#   2029L, "female", "afr-amer",      2L,       1L,    0L,     0L,     2L, 51.05624, 41.31353,     52L,     34L,      0L,       0L,      79L,       6L,
#   2030L, "female",    "white",      2L,       1L,    2L,     3L,     3L, 64.20476, 67.72454,     75L,     80L,      0L,       0L,      70L,      16L,
#   2031L,   "male",    "asian",      2L,       2L,    3L,     4L,     3L, 58.11485,  78.2363,     65L,     91L,      1L,       0L,      86L,       0L,
#   2032L, "female", "afr-amer",      2L,       0L,    2L,     2L,     3L, 46.82059, 42.45086,     44L,     36L,      0L,       0L,      77L,       8L,
#   2033L,   "male",    "white",      2L,       0L,    1L,     3L,     2L, 46.82059, 52.64643,     44L,     55L,      0L,       0L,      85L,       1L,
#   2034L, "female",    "white",      2L,       3L,    3L,     4L,     4L, 66.98323, 86.86945,     79L,     96L,      0L,       0L,      84L,       2L,
#   2035L, "female",    "white",      2L,       0L,    0L,     3L,     2L, 32.27546, 51.05624,     20L,     52L,      1L,       0L,      83L,       3L,
#   2036L, "female", "hispanic",      2L,       1L,    2L,     2L,     3L, 29.05672, 29.05672,     16L,     16L,      3L,       3L,      82L,       4L,
#   2037L, "female", "hispanic",      2L,       1L,    3L,     2L,     2L, 98.99289, 42.45086,     99L,     36L,      3L,       2L,      84L,       2L,
#   2038L, "female", "afr-amer",      2L,       3L,    2L,     4L,     2L, 61.65428, 84.64062,     71L,     95L,      0L,       0L,      82L,       4L,
#   2039L,   "male",    "white",      2L,       2L,    4L,     2L,     2L, 47.88486, 37.09416,     46L,     27L,      2L,       2L,      79L,       7L,
#   2040L, "female", "hispanic",      2L,       2L,    3L,     2L,     2L, 53.71444, 56.98883,     57L,     63L,      1L,       0L,      86L,       0L,
#   2041L, "female", "hispanic",      2L,       2L,    4L,     2L,     3L, 59.26457, 51.58518,     67L,     53L,      2L,       3L,      85L,       1L,
#   2042L, "female",    "white",      2L,       4L,    2L,     4L,     4L, 73.72179, 93.25195,     87L,     98L,      0L,       0L,      85L,       1L,
#   2043L, "female",    "white",      2L,       2L,    2L,     2L,     3L, 61.04388, 51.58518,     70L,     53L,      0L,       0L,      86L,       0L,
#   2044L, "female",    "white",      2L,       0L,    1L,     1L,     1L, 33.01677,  45.2079,     21L,     41L,      0L,       0L,      73L,      13L,
#   2045L, "female",    "white",      2L,       0L,    3L,     1L,     2L, 39.55739, 41.31353,     31L,     34L,      0L,       0L,      85L,       1L,
#   2046L,   "male",    "white",      2L,       3L,    2L,     4L,     2L, 56.98883, 62.27464,     63L,     72L,      2L,       0L,      86L,       0L,
#   2047L, "female", "hispanic",      2L,       2L,    4L,     3L,     3L, 46.82059, 47.88486,     44L,     46L,      2L,       2L,      85L,       1L,
#   2048L,   "male",    "white",      2L,       2L,    2L,     3L,     2L, 41.31353, 44.66451,     34L,     40L,      3L,       0L,      86L,       0L,
#   2049L, "female", "afr-amer",      2L,       2L,    3L,     3L,     4L, 65.56011, 71.82729,     77L,     85L,      0L,       0L,      85L,       1L,
#   2050L, "female",    "white",      2L,       1L,    2L,     3L,     3L, 82.74353, 98.99289,     94L,     99L,      1L,       0L,      86L,       0L,
#   2051L, "female", "afr-amer",      2L,       0L,    2L,     3L,     3L,  45.2079, 44.11754,     41L,     39L,      0L,       0L,      85L,       1L,
#   2052L, "female",    "white",      2L,       2L,    3L,     3L,     4L, 59.26457, 61.04388,     67L,     70L,      1L,       0L,      86L,       0L,
#   2053L, "female",    "white",      2L,       1L,    2L,     2L,     2L, 55.88246, 56.98883,     61L,     63L,      1L,       0L,      84L,       2L,
#   2054L,   "male",    "white",      2L,       4L,    3L,     4L,     2L, 69.27759, 79.59081,     82L,     92L,      0L,       0L,      86L,       0L,
#   2055L, "female", "afr-amer",      2L,       3L,    3L,     3L,     2L, 56.98883, 59.26457,     63L,     67L,      0L,       0L,      85L,       1L,
#   2056L,   "male",    "white",      2L,       3L,    4L,     3L,     2L, 56.98883, 47.35357,     63L,     45L,      3L,       1L,      86L,       0L,
#   2057L, "female",    "white",      2L,       3L,    4L,     4L,     3L, 65.56011, 62.27464,     77L,     72L,      1L,       0L,      82L,       4L,
#   2058L,   "male",    "white",      2L,       1L,    2L,     3L,     2L, 47.88486, 51.05624,     46L,     52L,      0L,       0L,      83L,       3L,
#   2059L,   "male",    "white",      2L,       1L,    2L,     2L,     2L, 20.40919, 40.15026,      8L,     32L,      1L,       0L,      85L,       1L,
#   2060L, "female", "hispanic",      2L,       3L,    4L,     3L,     4L, 55.88246, 81.08016,     61L,     93L,      1L,       0L,      86L,       0L,
#   2061L, "female",    "asian",      2L,       3L,    1L,     2L,     3L, 61.65428, 60.44261,     71L,     69L,      0L,       0L,      85L,       1L,
#   2062L, "female",    "white",      2L,       3L,    4L,     4L,     4L, 58.11485, 68.48849,     65L,     81L,      0L,       0L,      82L,       4L,
#   2063L, "female", "hispanic",      2L,       1L,    4L,     2L,     3L,  45.2079, 47.35357,     41L,     45L,      1L,       0L,      85L,       1L,
#   2064L,   "male", "hispanic",      2L,       0L,    0L,     0L,     0L, 29.05672, 32.27546,     16L,     20L,      2L,       2L,      77L,       9L,
#   2065L, "female",    "white",      2L,       2L,    2L,     3L,     3L, 33.73761, 59.84974,     22L,     68L,      0L,       0L,      86L,       0L,
#   2066L,   "male", "hispanic",      2L,       1L,    1L,     1L,     2L, 53.71444, 40.15026,     57L,     32L,      2L,       2L,      82L,       4L,
#   2067L, "female",    "asian",      2L,       3L,    2L,     3L,     1L, 50.52795, 53.71444,     51L,     57L,      2L,       0L,      54L,       8L,
#   2068L,   "male", "hispanic",      2L,       0L,    2L,     1L,     0L, 63.54885, 54.25188,     74L,     58L,      0L,       1L,      73L,      13L,
#   2069L,   "male",    "white",      2L,       1L,    4L,     2L,     3L,  45.2079, 48.94376,     41L,     48L,      2L,       0L,      86L,       0L,
#   2070L,   "male",    "white",      2L,       2L,    2L,     1L,     4L, 73.72179, 56.43343,     87L,     62L,      2L,       0L,      86L,       0L,
#   2071L,   "male",    "white",      2L,       3L,    4L,     4L,     4L, 59.26457, 49.47205,     67L,     49L,      0L,       0L,      86L,       0L,
#   2072L, "female",    "asian",      2L,       4L,    4L,     3L,     2L, 98.99289, 69.27759,     99L,     82L,      2L,       0L,      86L,       0L,
#   2073L, "female", "hispanic",      2L,       0L,    3L,     3L,     4L, 56.98883, 74.74522,     63L,     88L,      1L,       0L,      84L,       2L,
#   2074L, "female",    "white",      2L,       2L,    4L,     3L,     3L, 58.11485, 58.11485,     65L,     65L,      0L,       0L,      81L,       5L,
#   2075L, "female",    "white",      2L,       4L,    1L,     4L,     2L, 61.65428, 86.86945,     71L,     96L,      0L,       0L,      85L,       1L,
#   2076L,   "male",    "white",      2L,       2L,    2L,     4L,     2L, 65.56011, 66.98323,     77L,     79L,      1L,       0L,      86L,       0L,
#   2077L, "female", "afr-amer",      2L,       1L,    3L,     1L,     2L, 59.26457, 56.98883,     67L,     63L,      0L,       0L,      86L,       0L,
#   2078L, "female", "hispanic",      2L,       3L,    4L,     4L,     2L, 66.98323, 63.54885,     79L,     74L,      2L,       0L,      85L,       1L,
#   2079L, "female",    "white",      2L,       1L,    3L,     3L,     1L, 32.27546,       50,     20L,     50L,      2L,       0L,      84L,       2L,
#   2080L,   "male", "afr-amer",      2L,       3L,    4L,     3L,     3L, 70.94328, 73.72179,     84L,     87L,      0L,       0L,      84L,       1L,
#   2081L,   "male",    "asian",      2L,       0L,    3L,     4L,     1L, 37.72536, 37.72536,     28L,     28L,      2L,       0L,      81L,       5L,
#   2082L, "female",    "white",      2L,       0L,    2L,     1L,     3L, 32.27546, 29.90528,     20L,     17L,      2L,       0L,      83L,       3L,
#   2083L,   "male",    "white",      2L,       0L,    0L,     2L,     3L, 36.45115, 32.27546,     26L,     20L,      1L,       0L,      85L,       1L,
#   2084L, "female",    "asian",      2L,       1L,    3L,     2L,     4L, 64.20476, 71.82729,     75L,     85L,      0L,       0L,      83L,       3L,
#   2085L, "female",    "white",      2L,       1L,    4L,     1L,     3L, 56.98883, 71.82729,     63L,     85L,      0L,       0L,      80L,       6L,
#   2086L,   "male",    "white",      2L,       0L,    4L,     2L,     1L, 44.11754, 24.16932,     39L,     11L,      1L,       0L,      78L,       8L,
#   2087L,   "male",    "asian",      2L,       0L,    2L,     0L,     3L, 40.15026, 40.15026,     32L,     32L,      1L,       2L,      65L,      21L,
#   2088L,   "male", "hispanic",      2L,       0L,    1L,     0L,     1L, 28.17271, 30.72241,     15L,     18L,      1L,       0L,      19L,       1L,
#   2089L, "female",    "white",      2L,       2L,    2L,     3L,     2L, 52.64643, 79.59081,     55L,     92L,      0L,       0L,      79L,       7L,
#   2090L, "female",    "asian",      2L,       0L,    2L,     3L,     2L, 40.15026, 44.66451,     32L,     40L,      0L,       0L,      81L,       5L,
#   2091L,   "male",    "asian",      2L,       1L,    4L,     2L,     2L, 41.31353, 40.15026,     34L,     32L,      2L,       4L,      85L,       1L,
#   2092L, "female",    "white",      2L,       0L,    2L,     3L,     2L, 59.26457, 89.60951,     67L,     97L,      0L,       0L,      85L,       1L,
#   2093L, "female",    "white",      2L,       4L,    4L,     4L,     3L,  45.2079, 46.82059,     41L,     44L,      2L,       0L,      86L,       0L,
#   2094L,   "male",    "white",      2L,       0L,    2L,     1L,     2L,  45.2079, 53.17941,     41L,     56L,      2L,       0L,      82L,       4L,
#   2095L, "female", "hispanic",      2L,       1L,    4L,     2L,     3L, 38.34572, 55.33549,     29L,     60L,      0L,       0L,      86L,       0L,
#   2096L,   "male",    "white",      2L,       0L,    3L,     0L,     3L, 46.28556, 55.33549,     43L,     60L,      0L,       0L,      72L,      14L,
#   2097L, "female", "hispanic",      2L,       2L,    2L,     2L,     2L, 65.56011, 51.05624,     77L,     52L,      2L,       3L,      44L,       2L,
#   2098L, "female",    "white",      2L,       1L,    3L,     2L,     3L, 51.58518, 65.56011,     53L,     77L,      2L,       0L,      84L,       2L,
#   2099L, "female",    "white",      2L,       2L,    4L,     3L,     4L, 46.82059, 68.48849,     44L,     81L,      2L,       0L,      84L,       2L,
#   2100L,   "male",    "white",      2L,       3L,    3L,     2L,     4L, 50.52795, 43.01117,     51L,     37L,      0L,       0L,      86L,       0L,
#   2101L,   "male",    "white",      2L,       2L,    3L,     3L,     3L, 59.26457, 55.33549,     67L,     60L,      1L,       1L,      85L,       1L,
#   2102L,   "male",    "white",      2L,       0L,    2L,     0L,     0L, 49.47205, 43.01117,     49L,     37L,      0L,       0L,      67L,      19L,
#   2103L, "female",    "asian",      2L,       1L,    2L,     3L,     2L, 43.01117, 46.82059,     37L,     44L,      0L,       0L,      84L,       2L,
#   2104L,   "male",    "white",      2L,       1L,    2L,     3L,     3L, 58.11485, 71.82729,     65L,     85L,      0L,       0L,      74L,      11L,
#   2105L,   "male", "afr-amer",      2L,       2L,    2L,     3L,     3L, 46.28556, 70.94328,     43L,     84L,      0L,       0L,      86L,       0L,
#   2106L, "female", "hispanic",      2L,       3L,    2L,     3L,     3L, 48.94376, 53.71444,     48L,     57L,      2L,       2L,      81L,       5L,
#   2107L, "female", "afr-amer",      2L,       3L,    3L,     3L,     2L, 98.99289, 56.98883,     99L,     63L,      0L,       0L,      72L,      13L,
#   2108L, "female",    "white",      2L,       0L,    2L,     2L,     3L, 41.31353, 50.52795,     34L,     51L,      1L,       0L,      86L,       0L,
#   2109L,   "male",    "white",      2L,       3L,    4L,     3L,     2L, 73.72179, 82.74353,     87L,     94L,      1L,       0L,      81L,       5L,
#   2110L,   "male",    "white",      2L,       2L,    1L,     1L,     3L, 64.20476, 65.56011,     75L,     77L,      0L,       0L,      83L,       3L,
#   2111L, "female", "hispanic",      2L,       0L,    1L,     1L,     1L, 53.71444, 51.58518,     57L,     53L,      2L,       2L,      84L,       2L,
#   2112L,   "male",    "white",      2L,       3L,    2L,     3L,     3L, 55.88246, 98.99289,     61L,     99L,      0L,       0L,      84L,       2L,
#   2113L,   "male", "afr-amer",      2L,       0L,    2L,     3L,     1L, 62.90584, 56.98883,     73L,     63L,      0L,       0L,      81L,       5L,
#   2114L, "female",    "white",      2L,       3L,    4L,     4L,     4L, 61.04388, 71.82729,     70L,     85L,      1L,       0L,      86L,       0L,
#   2115L, "female",    "white",      2L,       4L,    4L,     3L,     4L, 98.99289, 98.99289,     99L,     99L,      0L,       0L,      83L,       3L,
#   2116L,   "male",    "white",      2L,       2L,    3L,     3L,     2L, 62.90584,  54.7921,     73L,     59L,      2L,       0L,      86L,       0L,
#   2117L,   "male",    "white",      2L,       2L,    3L,     1L,     2L, 58.68647, 60.44261,     66L,     69L,      0L,       0L,      85L,       1L,
#   2118L,   "male", "hispanic",      2L,       2L,    1L,     1L,     0L, 53.71444, 35.12527,     57L,     24L,      3L,       1L,      77L,       9L,
#   2119L,   "male",    "white",      2L,       3L,    2L,     2L,     1L, 65.56011, 44.11754,     77L,     39L,      0L,       0L,      85L,       1L,
#   2120L,   "male",    "white",      2L,       1L,    3L,     1L,     3L, 51.58518, 40.15026,     53L,     32L,      3L,       0L,      86L,       0L,
#   2121L,   "male",    "white",      2L,       0L,    2L,     2L,     3L, 62.90584, 62.27464,     73L,     72L,      0L,       0L,      86L,       0L,
#   2122L,   "male",    "white",      2L,       3L,    3L,     2L,     2L, 70.94328, 58.11485,     84L,     65L,      1L,       0L,      85L,       1L,
#   2123L,   "male", "hispanic",      2L,       0L,    1L,     2L,     0L, 70.09472, 48.41482,     83L,     47L,      0L,       0L,      73L,      12L,
#   2124L, "female",    "white",      2L,       1L,    2L,     3L,     3L, 65.56011, 62.27464,     77L,     72L,      0L,       0L,      83L,       3L,
#   2125L,   "male",    "white",      2L,       2L,    2L,     3L,     3L, 46.82059, 65.56011,     44L,     77L,      2L,       2L,      86L,       0L,
#   2126L, "female",    "white",      2L,       3L,    1L,     2L,     2L, 66.98323, 51.58518,     79L,     53L,      0L,       0L,      85L,       1L,
#   2127L,   "male", "afr-amer",      2L,       2L,    3L,     3L,     4L, 51.58518, 59.26457,     53L,     67L,      0L,       0L,      81L,       5L,
#   2128L, "female",    "white",      2L,       1L,    3L,     3L,     2L, 46.28556, 64.20476,     43L,     75L,      1L,       0L,      85L,       1L,
#   2129L,   "male",    "white",      2L,       1L,    4L,     3L,     2L, 47.88486, 58.68647,     46L,     66L,      0L,       0L,      85L,       1L,
#   2130L, "female", "hispanic",      2L,       1L,    3L,     3L,     3L, 41.88515, 58.11485,     35L,     65L,      2L,       1L,      78L,       7L,
#   2131L, "female",    "white",      2L,       1L,    3L,     4L,     3L, 56.98883, 59.84974,     63L,     68L,      0L,       0L,      80L,       6L,
#   2132L, "female",    "white",      2L,       2L,    4L,     2L,     3L, 64.20476, 81.08016,     75L,     93L,      2L,       2L,      86L,       0L,
#   2133L, "female",    "white",      2L,       1L,    1L,     2L,     2L, 70.94328, 58.68647,     84L,     66L,      2L,       0L,      78L,       8L,
#   2134L,   "male", "hispanic",      2L,       1L,    3L,     0L,     2L,  45.2079, 57.54914,     41L,     64L,      2L,       4L,      86L,       0L,
#   2135L, "female",    "white",      2L,       1L,    3L,     2L,     1L, 40.15026, 48.41482,     32L,     47L,      2L,       0L,      84L,       1L,
#   2136L,   "male", "hispanic",      2L,       1L,    2L,     2L,     2L, 25.25478, 13.13055,     12L,      4L,      3L,       2L,      86L,       0L,
#   2137L,   "male",    "white",      2L,       3L,    3L,     3L,     3L, 55.88246, 86.86945,     61L,     96L,      0L,       0L,      82L,       4L,
#   2138L, "female",    "asian",      2L,       0L,    1L,     0L,     0L, 98.99289, 93.25195,     99L,     98L,      1L,       0L,      63L,      17L,
#   2139L,   "male", "nat-amer",      2L,       2L,    2L,     1L,     2L, 59.26457, 44.11754,     67L,     39L,      0L,       0L,      80L,       6L,
#   2140L, "female",    "white",      2L,       1L,    4L,     2L,     3L, 41.88515, 41.31353,     35L,     34L,      1L,       0L,      86L,       0L,
#   2141L, "female",    "white",      2L,       4L,    4L,     4L,     4L, 61.04388, 56.98883,     70L,     63L,      2L,       0L,      86L,       0L,
#   2142L, "female",    "white",      2L,       2L,    2L,     3L,     2L, 73.72179, 70.94328,     87L,     84L,      0L,       0L,      85L,       1L,
#   2143L,   "male",    "white",      2L,       3L,    4L,     3L,     2L, 61.04388, 47.88486,     70L,     46L,      2L,       0L,      83L,       3L,
#   2144L,   "male", "hispanic",      2L,       0L,    2L,     3L,     2L, 53.71444, 61.04388,     57L,     70L,      1L,       0L,      84L,       1L,
#   2145L,   "male",    "white",      2L,       2L,    3L,     4L,     3L, 46.82059, 59.26457,     44L,     67L,      1L,       0L,      84L,       2L,
#   2146L, "female",    "white",      2L,       1L,    2L,     2L,     1L, 36.45115, 46.82059,     26L,     44L,      3L,       0L,      84L,       2L,
#   2147L,   "male",    "white",      2L,       0L,    2L,     2L,     3L, 56.98883, 61.04388,     63L,     70L,      0L,       0L,      84L,       2L,
#   2148L, "female", "hispanic",      2L,       0L,    1L,     2L,     0L, 20.40919, 15.35938,      8L,      5L,      2L,       4L,      81L,       5L,
#   2149L, "female", "hispanic",      2L,       0L,    3L,     0L,     3L, 47.88486,  54.7921,     46L,     59L,      1L,       0L,      45L,      41L,
#   2150L,   "male",    "white",      2L,       2L,    4L,     2L,     2L, 56.98883, 43.01117,     63L,     37L,      2L,       2L,      83L,       3L,
#   2151L, "female", "filipino",      2L,       0L,    2L,     2L,     0L,  54.7921, 71.82729,     59L,     85L,      1L,       0L,      79L,       7L,
#   2152L, "female",    "white",      2L,       3L,    3L,     2L,     3L, 47.88486, 69.27759,     46L,     82L,      0L,       0L,      85L,       1L,
#   2153L,   "male", "hispanic",      2L,       1L,    2L,     0L,     2L, 36.45115, 47.88486,     26L,     46L,      2L,       0L,      85L,       1L,
#   2154L, "female",    "white",      2L,       2L,    3L,     3L,     2L, 66.98323, 68.48849,     79L,     81L,      2L,       0L,      83L,       3L,
#   2155L, "female", "hispanic",      2L,       2L,    3L,     2L,     4L,  54.7921, 53.17941,     59L,     56L,      0L,       0L,      86L,       0L,
#   2156L, "female",    "white",      2L,       3L,    3L,     4L,     3L, 76.98948, 69.27759,     90L,     82L,      0L,       0L,      86L,       0L,
#   2157L, "female",    "white",      2L,       2L,    2L,     4L,     3L, 65.56011, 70.94328,     77L,     84L,      0L,       0L,      84L,       2L
# )
