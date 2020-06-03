#' @title
#' Sample of the High School and Beyond data set
#'
#' @description
#' A subest of the hsb data set containing demographic information and
#' standardized test scores of high school students.
#'
#' @format A data frame with 200 rows and 6 variables:
#' \describe{
#'   \item{female}{Gender of the student. Female = 1 / Not female = 0}
#'   \item{read}{Scores from test of reading}
#'   \item{write}{Scores from test of writing}
#'   \item{math}{Scores from test of math}
#'   \item{hon}{Student is in honors class (1) or not (0)}
#'   \item{femalexmath}{Interaction term between `female` and `math` variables}
#'   ...
#' }
#'
#' @source
#' \url{https://stats.idre.ucla.edu/wp-content/uploads/2016/02/sample.csv}
"hsb_sample"

