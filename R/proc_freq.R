

#### Proc freq for R --------------------------------

#' @title
#' A "PROC FREQ" for R
#'
#' @description
#' An R function to do some of the calcuations thatt SAS's PROC FREQ does. I
#' found this function on page 10 of _An Introduction to Categorical Data
#' Analysis Using R_ by Brett Presnell. It's just too fun not to include this
#' somehow. See references for link.
#'
#' @references
#' /url{http://users.stat.ufl.edu/~presnell/Courses/sta4504-2000sp/R/R-CDA.pdf}
#'
#' @import vcd
#' @importFrom stats pchisq
#'
#' @param x A table or matrix
#' @param digits Doesn't do anything that I can tell
#'
#' @return A list
#' @export
#'
#' @examples
#' jobsatis <- c(2,4,13,3, 2,6,22,4, 0,1,15,8, 0,3,13,8)
#' jobsatis <- matrix(jobsatis,byrow=TRUE,nrow=4)
#' dimnames(jobsatis) <- list(
#'   Income=c("<5","5-15","15-25",">25"),
#'   Satisfac=c("VD","LS","MS","VS"))
#' jobsatis
#' class(jobsatis)
#'
#' jobsat_freq <- proc_freq(jobsatis)
#' jobsat_freq
#'
#' library(vcd)
#' SexualFun
#' class(SexualFun)
#'
#' proc_freq(SexualFun)
#'
proc_freq <- function(x, digits = 4) {

  if (!(inherits(x, "matrix") | inherits(x, "table")))
    stop("Input must be a table or a matrix.")

  total <- sum(x)
  rowsum <- apply(x, 1, sum)
  colsum <- apply(x, 2, sum)
  prop <- x / total
  rowprop <- sweep(x, 1, rowsum, "/")
  colprop <- sweep(x, 2, colsum, "/")
  expected <- (matrix(rowsum) %*% t(matrix(colsum))) / total
  dimnames(expected) <- dimnames(x)
  resid <- (x - expected) / sqrt(expected)
  adj_resid <- resid /
    sqrt((1 - matrix(rowsum) / total) %*% t(1 - matrix(colsum) / total))
  df <- prod(dim(x) - 1)
  X2 <- sum(resid^2)
  attr(X2, "P-value") <- 1 - pchisq(X2, df)
  ## Must be careful about zero freqencies. Want 0*log(0) = 0.
  tmp <- x * log(x / expected)
  tmp[x == 0] <- 0
  G2 <- 2 * sum(tmp)
  attr(G2, "P-value") <- 1 - pchisq(G2, df)
  list(
    sample_size = total,
    row_totals = rowsum,
    col_totals = colsum,
    overall_proportions = prop,
    row_proportions = rowprop,
    col_proportions = colprop,
    expected_freqs = expected,
    residuals = resid,
    adjusted_residuals = adj_resid,
    chi_square = X2,
    likelihood_ratio_stat = G2,
    df = df
  )
}



