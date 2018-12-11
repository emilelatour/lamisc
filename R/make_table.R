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
#' @param unique_lvls Logical; To use the unique values of the variables that
#'   exist in the data use `TRUE` (default). To use the _levels_ attributed to
#'   the variables as a class factor then choose `FALSE` which may not actually
#'   exist in the data as actual values.
#' @param sorted Logical; If `FALSE` (default) then the values for the table
#'   appear just as they are in the data or as they are defined as a factor. If
#'   `TRUE`, then the factor order is overrided and the variables are sorted.
#' @param useNA useNA controls if the table includes counts of NA values: the
#'   allowed values correspond to never ("no"), only if the count is positive
#'   ("ifany") and even for zero counts ("always")
#'
#' @usage
#' make_table(data, x, y,
#'            x_lvls = NULL, y_lvls = NULL,
#'            labs = c(NA, NA),
#'            unique_lvls = TRUE, sorted = FALSE, useNA = "ifany")
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
#'
#' make_table(data = df, x = a, y = b)
#'
#' make_table(data = df, x = a, y = b, labs = c("Cats", "Dogs"))
#'
#' make_table(data = df,
#'            x = a,
#'            y = b,
#'            x_lvls = c(1, 0),
#'            y_lvls = c(1, 0),
#'            unique_lvls = FALSE)
#'
#' make_table(data = df,
#'            x = a,
#'            y = b,
#'            x_lvls = c(0, 1),
#'            y_lvls = c(0, 1),
#'            unique_lvls = FALSE)
#'
#'
#' make_table(data = df,
#'            x = a,
#'            y = b,
#'            x_lvls = c(1, 0),
#'            y_lvls = c(1, 0),
#'            unique_lvls = TRUE,
#'            sorted = TRUE)
#'
#'
#' make_table(data = df,
#'            x = a,
#'            y = b,
#'            x_lvls = c(1),
#'            y_lvls = c(1),
#'            unique_lvls = TRUE,
#'            sorted = TRUE)
make_table <- function(data,
                       x,
                       y,
                       x_lvls = NULL,
                       y_lvls = NULL,
                       labs = c(NA, NA),
                       unique_lvls = TRUE,
                       sorted = FALSE,
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
  if (!is.null(x_lvls)) {
    data <- data %>%
      mutate(!! x_name := factor(!! x_enq, levels = x_lvls))
  }

  if (!is.null(y_lvls)) {
    data <- data %>%
      mutate(!! y_name := factor(!! y_enq, levels = y_lvls))
  }


  ## Ensure the same factor levels ----------------
  # If the variables selected for the table do not have the same values then the
  # table will not be square. The following section will take the union of
  # values (either using the unique function or the levels function).

  if (unique_lvls == TRUE) {
    same_levels <- union(as.character(unique(data[[x_name]])),
                         as.character(unique(data[[y_name]])))
  } else {
    same_levels <- union(levels(unique(data[[x_name]])),
                         levels(unique(data[[y_name]])))
  }

  if (sorted == TRUE) {
    same_levels <- sort(same_levels)
  }

  # data <- data %>%
  #   mutate(!! x_name := factor(!! x_enq, levels = same_levels),
  #          !! y_name := factor(!! y_enq, levels = same_levels))

  data <- data %>%
    mutate(x_name := factor(!! x_enq, levels = same_levels),
           y_name := factor(!! y_enq, levels = same_levels))


  ## Calculate the table ----------------

  # useNA controls if the table includes counts of NA values: the allowed values
  # correspond to never ("no"), only if the count is positive ("ifany") and even
  # for zero counts ("always")

  table(data[[x_name]], data[[y_name]],
        dnn = labs,
        useNA = useNA)

}

