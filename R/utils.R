

# Copied from tidyr/R/utils.R, to export the magrittr pipe
# Copied from janitor/R/utils.R

#' Pipe operator
#'
#' @description Exported from the magrittr package.  To learn more, run \code{?magrittr::`\%>\%`}.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @importFrom janitor tabyl
#' @importFrom janitor adorn_totals
#' @usage lhs \%>\% rhs
#' @examples
#' library(janitor)
#' library(dplyr)
#' mtcars %>%
#'   tabyl(carb, cyl) %>%
#'   adorn_totals()
NULL
