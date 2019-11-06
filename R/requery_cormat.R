#' @title
#' Correlation matrix
#'
#' @description
#' Calculate and easily visualize a correlation matrix. Function comes from \href{http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need}{Correlation matrix : An R function to do all you need}
#'
#' @param x matrix of data values
#' @param type Possible values are “lower” (default), “upper”, “full” or
#'   “flatten”. It displays the lower or upper triangular of the matrix, full or
#'   flatten matrix.
#' @param graph if TRUE, a correlogram or heatmap is generated to visualize the
#'   correlation matrix.
#' @param graphType Type of graphs. Possible values are “correlogram” or
#'   “heatmap”.
#' @param col colors to use for the correlogram or the heatmap.
#' @param ... Further arguments to be passed to cor() or cor.test() function.
#'
#' @importFrom corrplot corrplot
#' @importFrom corrplot corrMatOrder
#' @importFrom grDevices colorRampPalette
#' @importFrom stats cor
#' @importFrom stats cor.test
#' @importFrom stats symnum
#'
#' @return
#' A list containing the following: \describe{ \item{r}{table of correlation
#' coefficients} \item{p}{Table of p-values corresponding to the significance
#' levels of the correlations} \item{sym}{A representation of the correlation
#' matrix in which coefficients are replaced by symbols according to the
#' strength of the dependence} }
#' @export
#'
#' @examples
#' mydata <- mtcars[, c(1,3,4,5,6,7)]
#' head(mydata)
#'
#' # Computing the correlation matrix
#' rquery_cormat(mydata)
#'
#' # Upper triangle of the correlation matrix
#' rquery_cormat(mydata, type = "upper")
#'
#' # Full correlation matrix
#' rquery_cormat(mydata, type = "full")

rquery_cormat <- function(x,
                          type = c("lower", "upper", "full", "flatten"),
                          graph = TRUE,
                          graphType = c("correlogram", "heatmap"),
                          col = NULL,
                          ...) {


  # Define color
  if (is.null(col)) {

    col <- grDevices::colorRampPalette(c("#67001F",
                                         "#B2182B",
                                         "#D6604D",
                                         "#F4A582",
                                         "#FDDBC7",
                                         "#FFFFFF",
                                         "#D1E5F0",
                                         "#92C5DE",
                                         "#4393C3",
                                         "#2166AC",
                                         "#053061"))(200)

    col <- rev(col)
  }

  # Correlation matrix
  cormat <- signif(stats::cor(x, use = "complete.obs", ...), 2)
  pmat <- signif(cor.pmat(x, ...), 2)

  # Reorder correlation matrix
  ord <- corrplot::corrMatOrder(cormat, order = "hclust")
  cormat <- cormat[ord, ord]
  pmat <- pmat[ord, ord]

  # Replace correlation coeff by symbols
  sym <- stats::symnum(cormat, abbr.colnames = FALSE)

  # Correlogram
  if (graph & graphType[1] == "correlogram") {

    corrplot::corrplot(cormat,
                       type = ifelse(type[1] == "flatten", "lower", type[1]),
                       tl.col = "black",
                       tl.srt = 45,
                       col = col, ...)

  } else if (graph & graphType[1] == "heatmap") {

    stats::heatmap(cormat, col = col, symm = TRUE)

  }

  # Get lower/upper triangle
  if (type[1] == "lower") {
    cormat <- getLower.tri(cormat)
    pmat <- getLower.tri(pmat)
  }
  else if (type[1] == "upper") {
    cormat <- getUpper.tri(cormat)
    pmat <- getUpper.tri(pmat)
    sym = t(sym)
  }
  else if (type[1] == "flatten") {
    cormat <- flattenCorrMatrix(cormat, pmat)
    pmat = NULL
    sym = NULL
  }

  list(r = cormat, p = pmat, sym = sym)

}



#### Helper functions --------------------------------

# Compute the matrix of correlation p-values
cor.pmat <- function(x, ...) {
  mat <- as.matrix(x)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- stats::cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- colnames(mat)
  rownames(p.mat) <- colnames(mat)
  p.mat
}

# Get lower triangle of the matrix
getLower.tri <- function(mat) {
  upper <- mat
  upper[upper.tri(mat)] <- ""
  mat <- as.data.frame(upper)
  mat
}

# Get upper triangle of the matrix
getUpper.tri <- function(mat) {
  lt <- mat
  lt[lower.tri(mat)] <- ""
  mat <- as.data.frame(lt)
  mat
}

# Get flatten matrix
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(row = rownames(cormat)[row(cormat)[ut]],
             column = rownames(cormat)[col(cormat)[ut]],
             cor  = (cormat)[ut],
             p = pmat[ut])
}
