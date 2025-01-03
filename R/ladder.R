
#' @title
#' Ladder of powers
#'
#' @description
#' Shows Tukey's ladder of powers to identify a transform that converts `var`
#' into a normally distributed variable.
#'
#' This is intended to replicate the `ladder` command in Stata.
#'
#' @references
#' https://www.stata.com/manuals/rladder.pdf
#'
#' @param data A tibble or data frame.
#' @param var A numeric vector of data values.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom tibble tibble
#'
#' @return
#' A tibble.
#'
#' @export
#'
#' @examples
#'
#' ladder(data = auto,
#'        var = mpg)
#'
ladder <- function(data, var) {

  # Fix no visible binding for global variable
  adj_chi2 <- p_value <- transformation <- NULL


  var <- rlang::enquo(var)

  x <- data %>%
    dplyr::pull(!! var)

  if (any(is.na(x))) {
    warning("The variable contains NA values. These will be omitted from the analysis.")
  }

  # Remove NA values for further processing
  x <- x[!is.na(x)]

  transformed_x <- tibble::tibble(
    cubic = x ^ 3,
    square = x ^ 2,
    identity = x,
    square_root = sqrt(x),
    log = log(x),
    inv_square_root = 1 / sqrt(x),
    inverse = 1 / x,
    inv_square = 1 / (x ^ 2),
    inv_cubic = 1 / (x ^ 3)
  )

  # Get all the column names
  transformed_x_nms <- names(transformed_x)

  # Remove columns with any Inf values
  transformed_x <- transformed_x |>
    dplyr::select(dplyr::where(~ !any(is.infinite(.))))


  # Find the positions
  positions <- match(names(transformed_x), transformed_x_nms)


  # Adjust labels to match remaining columns
  labels <- c("Cubic",
              "Square",
              "Identity",
              "Square root",
              "Log",
              "1 / (Square root)",
              "Inverse",
              "1 / Square",
              "1 / Cubic")

  labels_x <- labels[positions]


  res <- purrr::map_df(.x = names(transformed_x),
                       .f = ~ lamisc::sktest(data = transformed_x,
                                             !! .x))

  res <- tibble::tibble(
    transformation = labels,
    var = transformed_x_nms
  ) |>
    dplyr::left_join(res,
                     by = "var") |>
    dplyr::select(transformation,
                  adj_chi2,
                  p_value) |>
    dplyr::mutate(p_value = lamisc::fmt_pvl(x = p_value))


  return(res)

}

