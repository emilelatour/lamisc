#' Clean SPSS Data Imported via `haven`
#'
#' This function cleans SPSS `.sav` data imported using `haven`, converting
#' labelled variables into factors and cleaning variable names. It also
#' creates a data dictionary and a list of factor variables with their levels
#' and labels.
#'
#' @param data A data frame or tibble imported with `haven::read_sav()`.
#' @param method A string specifying how to convert labelled variables to factors.
#'   Options are `"manual"` (uses `factor()` with `labels` attributes) or `"forcats"`
#'   (uses `forcats::as_factor()` with `levels = "labels"`). Default is `"manual"`.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{data}{A cleaned data frame with labelled variables converted to factors.}
#'   \item{dictionary}{A tibble showing variable names and labels.}
#'   \item{factor_vars}{A named list of variables with labels, showing levels and names.}
#' }
#'
#' @examples
#' \dontrun{
#' # Raw_data can be downloaded from here and saved to disc
#' # https://www.cambridge.org/us/academic/subjects/psychology/psychology-research-methods-and-statistics/statistics-using-ibm-spss-integrative-approach-3rd-edition?format=PB&isbn=9781107461222
#' raw_data <- haven::read_spss(file = "/Users/latour/Dropbox/code/R/qualtrics/Wages.sav")
#' cleaned <- clean_spss_data(raw_data, method = "forcats")
#' str(cleaned$data)
#'
#' # Apply labels to the data
#' labels_list <- stats::setNames(object = as.list(cleaned$dictionary$lbl),
#'                                nm = cleaned$dictionary$var)
#'
#' labelled::var_label(cleaned$data) <- labels_list
#'
#' # See that now there are attributes where the labels are stored.
#' str(cleaned$data)
#'
#'
#' }
#'
#' @importFrom dplyr mutate across one_of rename all_of
#' @importFrom janitor clean_names
#' @importFrom sjlabelled get_label
#' @importFrom tibble enframe
#' @importFrom purrr map
#' @importFrom forcats as_factor
#' @export
clean_spss_data <- function(data, method = c("manual", "forcats")) {

  method <- match.arg(method)

  #### Make a data dictionary --------------------------------------------

  original_col_nms <- names(data)
  data <- janitor::clean_names(data)

  data_dictionary <- data |>
    sjlabelled::get_label() |>
    tibble::enframe() |>
    dplyr::rename(
      var = name,
      lbl = value
    ) |>
    dplyr::mutate(var_orig = original_col_nms)

  #### Identify labelled variables ---------------------------------------

  variables_with_labels <- purrr::map(.x = data,
                                      .f = ~ any(attr(.x, "class") == "haven_labelled")) |>
    unlist() |>
    which() |>
    names()

  factor_vars <- variables_with_labels |>
    purrr::map(~ attr(data[[.x]], "labels"))
  names(factor_vars) <- variables_with_labels

  #### Convert to factors ------------------------------------------------

  if (method == "manual") {
    for (i in seq_along(variables_with_labels)) {
      x <- variables_with_labels[i]
      data[[x]] <- factor(data[[x]],
                          levels = factor_vars[[i]],
                          labels = names(factor_vars[[i]]))
    }
  } else if (method == "forcats") {
    data <- data |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(variables_with_labels),
        .fns = ~ forcats::as_factor(., levels = "labels")
      ))
  }

  #### Return result -----------------------------------------------------

  res <- list(
    data = data,
    dictionary = data_dictionary,
    factor_vars = factor_vars
  )

  return(res)
}
