

#' @title
#' Count and percentages
#'
#' @description
#' Wrapper function to `dplyr::count()` and `dplyr::add_count()`
#'
#' @param data  A tibble or data frame.
#' @param ... <data-masking> Variables to group by.
#' @param sort If TRUE, will show the largest groups at the top.
#' @param .drop For count(): if FALSE will include counts for empty groups (i.e. for levels of factors that don't exist in the data). Deprecated in add_count() since it didn't actually affect the output.
#'
#' @importFrom dplyr add_count
#' @importFrom dplyr count
#' @importFrom dplyr mutate
#' @importFrom rlang enquos
#'
#'
#' @return
#' An object of the same type as .data. count() and add_count() group transiently, so the output has the same groups as the input.
#'
#' @export
#'
#' @examples
#' library(palmerpenguins)
#'
#' count_count(data = penguins,
#'             species)
#'
#' count_count(data = penguins,
#'             species,
#'             sex)
#'
#' count_count(data = penguins,
#'             species,
#'             sex,
#'             sort = TRUE)

count_count <- function(data, ...,
                        sort = FALSE,
                        .drop = FALSE) {

  vars <- rlang::enquos(...)

  if (length(vars) > 2) {
    stop("only able to handle 2 variables max.")
    }

  data %>%
    dplyr::count(!!! vars,
                 sort = sort,
                 .drop = .drop) %>%
    dplyr::add_count(!!! vars[-length(vars)],
                     wt = n,
                     name = "nn") %>%
    dplyr::mutate(pct = n / nn)

}

