#' @title
#' More consistent version of base::table()
#'
#' @description
#' Takes a data frame and the columns specified and creates a table object that
#' is clean and easy to use for the rest of the calculations. It helps to
#' resolve some issues with the base R table function that doesn't handle empty
#' cells very well IMHO.
#'
#' @param data A data frame or tibble
#' @param x "X" variable; counts appear in the rows of the table
#' @param y "Y" variable; counts appear in the columns of the table
#' @param x_lvls (optional) levels for the X variable
#' @param y_lvls (optional) levels for the Y variable
#' @param labs (optional) labels for the X and Y variables
#' @param useNA useNA controls if the table includes counts of NA values: the
#'   allowed values correspond to never ("no"), only if the count is positive
#'   ("ifany") and even for zero counts ("always")
#'
#' @usage
#' make_table(data, x, y, x_lvls = NULL, y_lvls = NULL, labs = c(NA, NA), useNA = "ifany")
#'
#' @import dplyr
#' @import rlang
#' @import tibble
#'
#' @return A table
#' @export
#'
#'
#' @examples
#' df <- tibble::tribble(
#'   ~a, ~b, ~c,
#'   1L, 1L, 1L,
#'   0L, 1L, 1L,
#'   1L, 1L, 0L,
#'   0L, 1L, 0L,
#'   1L, 1L, 1L,
#'   0L, 1L, 1L,
#'   1L, 1L, 0L
#' )
#'
#' table(df$a, df$b)
#' make_table(data = df, x = a, y = b)
#' make_table(data = df, x = a, y = b, labs = c("Cats", "Dogs"))
make_table <- function(data,
                       x,
                       y,
                       x_lvls = NULL,
                       y_lvls = NULL,
                       labs = c(NA, NA),
                       useNA = "ifany") {

  # Note that y will show on the vertical side and x will show on the
  # horizontal (top)

  x_enq <- rlang::enquo(x)
  y_enq <- rlang::enquo(y)
  x_name <- rlang::quo_name(x_enq)
  y_name <- rlang::quo_name(y_enq)


  if (is.na(labs[1])) {
    labs[1] = x_name
  }

  if (is.na(labs[2])) {
    labs[2] = y_name
  }


  ## Check for factor levels as function argument ----------------
  # Check if the factor levels were given as arguments to the function
  data <- data %>%
    mutate(!! x_name := apply_fct_lvls_if_any(var = !! x_enq, lvl = x_lvls),
           !! y_name := apply_fct_lvls_if_any(var = !! y_enq, lvl = y_lvls))


  ## Use levels of factor with more levels ----------------
  # if one variable has more factor levels than the other, then use that one's
  # factor levels for both variables. This will help to ensure that the table
  # will have equal dimensions later.

  # Calc the larger number of factor levels and assign them to both variables.
  data <- data %>%
    mutate(!! x_name := make_fct_w_equal_lvls(a = !! x_enq, b = !! y_enq),
           !! y_name := make_fct_w_equal_lvls(a = !! y_enq, b = !! x_enq))


  ## Calculate the table ----------------

  # useNA controls if the table includes counts of NA values: the allowed values
  # correspond to never ("no"), only if the count is positive ("ifany") and even
  # for zero counts ("always")

  table(data[[x_name]], data[[y_name]],
        dnn = labs,
        useNA = useNA)

}


#### Helper functions for make_table --------------------------------

## apply_fct_lvls_if_any ---------------
# Check if any factor levels were supplied in the arguments to make_table
apply_fct_lvls_if_any <- function(var, lvl) {
  if (is.null(lvl)) {
    factor(var)
  } else {
    factor(var, levels = lvl)
  }
}

## make_fct_w_equal_lvls ---------------
# if one variable has more factor levels than the other, then use that one's
# factor levels for both variables. This will help to ensure that the table
# will have equal dimensions later.
make_fct_w_equal_lvls <- function(a, b) {
  if (length(levels(a)) >= length(levels(b))) {
    factor(a, levels = levels(a))
  } else {
    factor(a, levels = levels(b))
  }
}

