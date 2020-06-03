#' @title
#' Admission into graduate school
#'
#' @description
#' A researcher is interested in how variables, such as GRE (Graduate Record
#' Exam scores), GPA (grade point average) and prestige of the undergraduate
#' institution, effect admission into graduate school. The response variable,
#' admit/don’t admit, is a binary variable.
#'
#' This dataset has a binary response (outcome, dependent) variable called
#' `admit.` There are three predictor variables: `gre`, `gpa` and `rank.` We
#' will treat the variables `gre` and `gpa` as continuous. The variable `rank`
#' takes on the values 1 through 4. Institutions with a rank of 1 have the
#' highest prestige, while those with a rank of 4 have the lowest.
#'
#' @format A data frame with 400 rows and 4 variables:
#' \describe{
#'   \item{admit}{admit = 1 / don’t admit = 0}
#'   \item{gre}{Graduate Record Exam scores}
#'   \item{gpa}{grade point average}
#'   \item{rank}{1 = highest prestige, 4 = lowest prestige}
#'   ...
#' }
#'
#' @source
#' \url{https://stats.idre.ucla.edu/stat/data/binary.csv}
"admissions"
