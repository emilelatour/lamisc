
#' @title
#' Inverse logit function
#'
#' @description
#' Given a numeric object return the inverse logit of the values. Code adapted
#' from the `boot` package.
#'
#' @param logit A numeric object. Missing values (NAs) are allowed.
#'
#' @importFrom dplyr mutate
#' @importFrom stats coef
#' @importFrom stats predict
#' @importFrom tibble as_tibble
#'
#' @return
#' An object of the same type as `logit` containing the inverse logits of the input values.
#'
#' @details
#' The inverse logit is defined by `exp(logit) / (1 + exp(logit))`. Values in `x` of `-Inf` or `Inf`
#' return logits of `0` or `1` respectively. Any `NA`s in the input will also be `NA`s
#' in the output.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' library(titanic)
#' data(titanic_train, package = "titanic")
#' d <- titanic_train  # less typing
#' glm1 <- glm(Survived ~ Pclass, data = d, family = "binomial")
#' betas <- coef(glm1)
#'
#' d <- d %>%
#'   tibble::as_tibble() %>%
#'   mutate(logit = betas[1] + betas[2] * Pclass,
#'          prob = inv_logit(logit = logit))
#'
#' (preds <- predict(glm1, newdata = d, type = "response"))
#'
#' d$prob == preds


inv_logit <- function(logit) {
  prob <- exp(logit) / (1 + exp(logit))
  # prob <- 1 / (1 + exp(-logit))
  prob[logit == -Inf] <- 0
  prob[logit == Inf] <- 1
  return(prob)
}

# logit2prob <- function(logit){
#   odds <- exp(logit)
#   prob <- odds / (1 + odds)
#   return(prob)
# }
