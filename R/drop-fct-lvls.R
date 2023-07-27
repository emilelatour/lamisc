
#' @title
#' Drop factor levels and filter the data the same time
#'
#' @description
#' When you filter data to remove rows that match a level in a factor, the
#' factor levels aren't removed. This function will filter the data and drop the
#' factor levels.
#'
#' @param data A data frame or tibble
#' @param x A factor (or character vector).
#' @param lvls Character string of factor levels to drop
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom forcats fct_drop
#' @import rlang
#'
#' @return
#' An object of the same type as data.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(palmerpenguins)
#'
#' penguins %>%
#'   dplyr::count(species)
#'
#' penguins %>%
#'   drop_fct_lvls(data = .,
#'                 x = species,
#'                 lvls = c("Chinstrap")) %>%
#'   dplyr::count(species)
#'
#' penguins %>%
#'   drop_fct_lvls(data = .,
#'                 x = species,
#'                 lvls = c("Chinstrap",
#'                          "Gentoo")) %>%
#'   dplyr::count(species)
drop_fct_lvls <- function(data, x, lvls) {

  uniq_vals <- unique(dplyr::pull(.data = data, var = {{ x }}))
  uniq_lvls <- levels(dplyr::pull(.data = data, var = {{ x }}))
  uniq_vals <- union(uniq_vals, uniq_lvls)

  if (any(!lvls %in% uniq_vals)) {

    warning("Some levels don't appear in the column in the data")
  }

  curr_lvls <- dplyr::pull(.data = data,
                           var = {{ x }}) %>%
    levels()

  new_lvls <- curr_lvls[!curr_lvls %in% lvls]

  data <- data %>%
    dplyr::filter(! {{ x }} %in% lvls) %>%
    # dplyr::mutate({{ x }} := forcats::fct_drop({{ x }})) |>
    mutate({{ x }} := factor({{ x }},
                             levels = new_lvls))

  return(data)
}




