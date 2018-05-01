

#' More consistent version of base::table()
#'
#' Takes a data frame and the columns specified and creates a table object that
#' is clean and easy to use for the rest of the calculations. It helps to
#' resolve some issues with the base R table function that doesn't handle empty
#' cells very well IMHO.
#'
#' @param df A data frame or tibble
#' @param x_var X variable, the one along the horizontal (top) of the table
#' @param y_var Y variable, the one along the vertical (side) of the table
#' @param x_lvls (optional) levels for the X variable
#' @param y_lvls (optional) levels for the Y variable
#' @param labs (optional) labels for the X and Y variables
#' @param useNA useNA controls if the table includes counts of NA values: the
#'   allowed values correspond to never ("no"), only if the count is positive
#'   ("ifany") and even for zero counts ("always")
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
#' make_table(df = df, x_var = a, y_var = b)

make_table <- function(df,
                       x_var,
                       y_var,
                       x_lvls = NULL,
                       y_lvls = NULL,
                       labs = c(NA, NA),
                       useNA = "ifany") {

  # Note that y will show on the vertical side and x will show on the
  # horizontal (top)

  x_enq <- rlang::enquo(x_var)
  y_enq <- rlang::enquo(y_var)
  x_name <- rlang::quo_name(x_enq)
  y_name <- rlang::quo_name(y_enq)


  if (is.na(labs[2])) {
    labs[2] = x_name
  }

  if (is.na(labs[1])) {
    labs[1] = y_name
  }


  ## Check for factor levels as function argument ----------------

  # Helper function to make code below more readable
  apply_fct_lvls_if_any <- function(var, lvl) {
    if (is.null(lvl)) {
      factor(var)
    } else {
      factor(var, levels = lvl)
    }
  }

  # Check if the factor levels were given as arguments to the function
  df <- df %>%
    mutate(!! x_name := apply_fct_lvls_if_any(var = !! x_enq, lvl = x_lvls),
           !! y_name := apply_fct_lvls_if_any(var = !! y_enq, lvl = y_lvls))


  ## Use levels of factor with more levels ----------------

  # if one variable has more factor levels than the other, then use that one's
  # factor levels for both variables. This will help to ensure that the table
  # will have equal dimensions later.


  # Function to make code below more readable
  make_fct_w_equal_lvls <- function(a, b) {
    if (length(levels(a)) >= length(levels(b))) {
      factor(a, levels = levels(a))
    } else {
      factor(a, levels = levels(b))
    }
  }

  # Calc the larger number of factor levels and assign them to both variables.
  df <- df %>%
    mutate(!! x_name := make_fct_w_equal_lvls(a = !! x_enq, b = !! y_enq),
           !! y_name := make_fct_w_equal_lvls(a = !! y_enq, b = !! x_enq))


  ## Calculate the table ----------------

  # useNA controls if the table includes counts of NA values: the allowed values
  # correspond to never ("no"), only if the count is positive ("ifany") and even
  # for zero counts ("always")

  table(df[[y_name]], df[[x_name]],
        dnn = labs,
        useNA = useNA)

}
