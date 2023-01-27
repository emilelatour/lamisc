


#### Packages -----------------------------

pacman::p_load(
  tidyverse,     # packages ggplot2, dplyr, tidyr, readr, purrr, tibble,
  # stringr, and forcats
  broom,         # functions tidy(), glance(), augment()
  fs,            # Cross-platform interface to file system operations
  glue,          # Glue strings to data in R
  here,          # Constructs paths to your project's files
  janitor,       # for working with dirty data
  lubridate,     # Functions to work with date-times and time-spans
  mice,          # Multiple imputation using Fully Conditional Specification
  naniar,        # structures, summaries, and visualisations for missing data
  readxl,        # read in excel files
  scales,        # Scale functions for visualization
  install = FALSE
)


#' @title
#' Drop factor levels and filter the data the same time
#'
#' @description
#' When you filter data to remove rows that match a level in a factor, the
#' factor levels aren't removed. This function will filter the data and drop the
#' factor levels.
#'
#' @param data A data frame or tibble. Or a survey object (class = `tbl_svy`,
#'   `survey.design2`, or `survey.design`)
#' @param var A factor (or character vector).
#' @param lvls_to_drop Character vector of factors to drop
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom purrr pluck
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom srvyr as_survey_design
#' @importFrom survey as.svydesign2
#'
#' @return
#' An object of the same type as data.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' library(palmerpenguins)
#' library(srvyr)
#' library(survey)
#'
#' # With a data frame
#' df <- tibble::tibble(letters = letters[1:5],
#'                      numbers = seq(1:5))
#'
#' f_lvls <- letters[1:6]
#'
#' df <- df %>%
#'   mutate(letters = factor(letters,
#'                           levels = f_lvls))
#'
#' levels(df$letters)
#'
#' df %>%
#'   drop_lvls2(data = .,
#'             var = letters,
#'             lvls = "a") %>%
#'   dplyr::count(letters,
#'                .drop = FALSE)
#'
#' df %>%
#'   drop_lvls2(data = .,
#'             var = letters,
#'             lvls = c("a", "b", "g")) %>%
#'   dplyr::count(letters,
#'                .drop = FALSE)
#'
#'
#' # With a tibble
#' penguins %>%
#'   dplyr::count(species)
#'
#' penguins %>%
#'   drop_lvls2(data = .,
#'             var = species,
#'             lvls = c("Chinstrap")) %>%
#'   dplyr::count(species)
#'
#' penguins %>%
#'   drop_lvls2(data = .,
#'             var = species,
#'             lvls = c("Chinstrap",
#'                      "Gentoo")) %>%
#'   dplyr::count(species)
#'
#'
#' # With a survey using srvyr package
#' data(api)
#'
#' srs_design_srvyr <- apisrs %>% as_survey_design(ids = 1, fpc = fpc)
#' srs_design_survey <- svydesign(ids = ~1, fpc = ~fpc, data = apisrs)
#'
#' srs_design_srvyr %>%
#'   group_by(stype) %>%
#'   summarize(proportion = survey_mean(),
#'             total = survey_total())
#'
#' srs_design_srvyr %>%
#'   drop_lvls2(data = .,
#'             var = stype,
#'             lvls_to_drop = "H") %>%
#'   group_by(stype) %>%
#'   summarize(proportion = survey_mean(),
#'             total = survey_total())
#'
#' srs_design_srvyr %>%
#'   drop_lvls2(data = .,
#'             var = stype,
#'             lvls_to_drop = c("H", "M")) %>%
#'   group_by(stype) %>%
#'   summarize(proportion = survey_mean(),
#'             total = survey_total())
#'
#' # With a survey using survey package
#' svymean(~stype, srs_design_survey)
#' svytotal(~stype, srs_design_survey)
#'
#' srs_design_survey2 <- srs_design_survey %>%
#'   drop_lvls2(data = .,
#'             var = stype,
#'             lvls_to_drop = c("H"))
#'
#' svymean(~stype, srs_design_survey2)
#' svytotal(~stype, srs_design_survey2)


drop_lvls2 <- function(data, var, lvls_to_drop = NULL) {

  var <- rlang::enquo(var)
  var_nm <- rlang::quo_name(var)

  is_srvyr <- any(class(data) %in% c("tbl_svy"))

  is_survey <- any(class(data) %in% c("survey.design2",
                                      "survey.design"))

  if (!is_srvyr & !is_survey) {

    lvls <- dplyr::pull(.data = data,
                        var = !! var) %>%
      levels()

    new_lvls <- lvls[!lvls %in% lvls_to_drop]

    data <- data %>%
      dplyr::filter(! (!! var %in% lvls_to_drop)) %>%
      mutate(!! var_nm := factor(!! var,
                                 levels = new_lvls))


  } else if (is_srvyr) {

    lvls <- purrr::pluck(data,
                         "variables",
                         var_nm) %>%
      levels()

    new_lvls <- lvls[!lvls %in% lvls_to_drop]

    data <- data %>%
      mutate(foo = dplyr::if_else(!! var %in% lvls_to_drop, 1, 0)) %>%
      subset(., foo != 1) %>%
      dplyr::select(-foo) %>%
      mutate(!! var_nm := factor(!! var,
                                 levels = new_lvls))

  } else if (is_survey) {

    lvls <- purrr::pluck(data,
                         "variables",
                         var_nm) %>%
      levels()

    new_lvls <- lvls[!lvls %in% lvls_to_drop]

    data <- data %>%
      srvyr::as_survey_design() %>%
      mutate(foo = dplyr::if_else(!! var %in% lvls_to_drop, 1, 0)) %>%
      subset(., foo != 1) %>%
      dplyr::select(-foo) %>%
      mutate(!! var_nm := factor(!! var,
                                 levels = new_lvls)) %>%
      survey::as.svydesign2()

  }


  return(data)

}

