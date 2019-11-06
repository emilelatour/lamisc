#' @title
#' Variance inflation factor (VIF)
#'
#' @description
#' Calculates variance-inflation linear, generalized linear, and other models.
#'
#' I found this function on
#' \href{https://stackoverflow.com/questions/33397689/multi-collinearity-for-categorical-variables}{StackOverflow}
#' The function `vif` from the `car` package would be my preference but I wanted to
#' put this one somewhere for posterity.
#'
#' @param fit An `lm` or `glm` object
#'
#' @importFrom Hmisc num.intercepts
#' @importFrom tibble enframe
#'
#' @return A tbl_df
#' @export
#'
#' @examples
#' library(car)
#'
#' # Using the car package
#' car::vif(lm(prestige ~ income + education, data = Duncan))
#' car::vif(lm(prestige ~ income + education + type, data = Duncan))
#'
#' # User defined function
#' calc_vif(lm(prestige ~ income + education, data = Duncan))
#' calc_vif(lm(prestige ~ income + education + type, data = Duncan))
#'
#' # Another example using glm
#' model1 <- glm(case ~ spontaneous + induced,
#'               data = infert,
#'               family = binomial(link = "logit"))
#' summary(model1)
#' car::vif(model1)
#' calc_vif(model1)
calc_vif  <- function(fit) {

  v <- vcov(fit, regcoef.only = TRUE)
  nam <- dimnames(v)[[1]]
  ns <- Hmisc::num.intercepts(fit)

  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }

  d <- diag(v) ^ 0.5
  v <- diag(solve(v / (d %o% d)))

  names(v) <- nam
  tibble::enframe(v,
                  name = "term",
                  value = "vif")

}


