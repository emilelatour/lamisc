
#' @title
#' Set variable labels
#'
#' @description
#' Set variable labels for a data frame or tibble. Basically a wrapper around
#' `labelled::var_label()` with a syntax that I can remember and some additional
#' warnings and fixes that I like to have.
#'
#' If a label is not given, then the variable name in the data is used as the
#' label.
#'
#' @param data A data.frame or tbl_df
#' @param vars A character vector of variable names.
#' @param labels A character vector of labels. Must be the same length as
#'   `vars`.
#'
#' @importFrom dplyr if_else
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom labelled var_label
#' @importFrom tibble tibble
#' @importFrom stats setNames
#'
#' @return
#' A labelled data.frame or a tbl_df.
#'
#' @export
#'
#' @examples
#' library(labelled)
#' # Load some packages for the example
#' library(survival)
#' library(dplyr)
#' library(tibble)
#'
#' # Data set comes from the survival package
#' data(pbc)
#'
#' # Tibble with labels
#' var_labels <- tibble::tribble(
#'   ~vars,                                      ~labels,
#'   "id",                                "Case Number",
#'   "time",          "Number of days since registration",
#'   "status",                         "Status at endpoint",
#'   "trt",                            "Treatment group",
#'   "age",                              "Age, in years",
#'   "sex",                                        "Sex",
#'   "ascites",                        "Presence of ascites",
#'   "hepato", "Presence of hepatomegaly or enlarged liver",
#'   "spiders",     "Blood vessel malformations in the skin",
#'   "edema",                          "Presence of edema",
#'   "bili",                   "Serum bilirunbin (mg/dl)",
#'   "chol",                  "Serum cholesterol (mg/dl)",
#'   "albumin",                       "Serum albumin (g/dl)",
#'   "copper",                      "Urine copper (ug/day)",
#'   "alk.phos",             "Alkaline phosphotase (U/liter)",
#'   "ast",          "Aspartate aminotransferase (U/ml)",
#'   "trig",                      "Triglycerides (mg/dl)",
#'   "platelet",                             "Platelet count",
#'   "protime",           "Standardised blood clotting time",
#'   "stage", "Histologic stage of disease (needs biopsy)"
#' )
#'
#'
#' pbc <- apply_data_labels(data = pbc,
#'                          vars = var_labels$vars,
#'                          labels = var_labels$labels)
#' labelled::var_label(pbc)
#' str(pbc)

apply_data_labels <- function(data,
                              vars,
                              labels) {

  if (length(vars) != length(labels)) {
    stop("'vars' and 'labels' must be the same length.")
  }

  if (any(!vars %in% names(data))) {
    warning("Some variables in 'vars' not found in 'data'.")
  }

  if (any(!names(data) %in% vars)) {
    warning("Some variables in 'data' not found in 'vars'.")
  }

  var_labels <- tibble::tibble(vars = names(data)) %>%
    dplyr::left_join(.,
                     tibble::tibble(vars = vars,
                                    labels = labels),
                     by = "vars") %>%
    dplyr::mutate(labels = dplyr::if_else(is.na(labels),
                                          vars,
                                          labels))


  labels_list <- stats::setNames(object = as.list(var_labels$labels),
                          nm = var_labels$vars)


  # Apply labels
  labelled::var_label(data) <- labels_list

  return(data)


}
