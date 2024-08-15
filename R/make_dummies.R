#' Create Dummy Variables from Categorical Columns
#'
#' The `make_dummies` function takes a data frame and a vector of variable names and returns a modified data frame where the specified categorical variables have been converted into dummy variables. The original variables are removed, and the dummy variables are added to the data frame with cleaned and consistent names.
#'
#' @param data A data frame containing the variables to be converted into dummy variables.
#' @param vars A character vector specifying the names of the categorical variables to be transformed into dummy variables.
#'
#' @return A data frame with the specified categorical variables replaced by their corresponding dummy variables.
#'
#' @details This function uses `model.matrix` to create dummy variables for the specified categorical variables in the data. The dummy variables are created without an intercept (`- 1`) and are then cleaned using `janitor::clean_names` to ensure consistent naming. The function also uses `tibble::as_tibble` to return the dummy variables in tibble format. The original variables specified in `vars` are removed from the data frame and replaced with the newly created dummy variables.
#'
#' @examples
#' # Example usage
#' data <- data.frame(
#'   gender = c("Male", "Female", "Female", "Male"),
#'   age = c(25, 30, 22, 40)
#' )
#' make_dummies(data, vars = c("gender"))
#'
#' @importFrom dplyr all_of bind_cols select
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom stats as.formula model.matrix
#' @importFrom tibble as_tibble
#'
#' @export
make_dummies <- function(data, vars) {

  for (i in seq_along(vars)) {
    x <- vars[i]

    form <- glue::glue(" ~ {x} - 1")

    dummy_df <- model.matrix(as.formula(form), data = data[x]) |>
      as.data.frame() |>
      janitor::clean_names()|>
      tibble::as_tibble()

    data <- data %>%
      dplyr::select(-dplyr::all_of(x)) |>
      dplyr::bind_cols(dummy_df)
  }

  return(data)
}
