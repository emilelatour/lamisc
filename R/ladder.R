
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
  adj_chi2 <- NULL
  p_value <- NULL



  var <- rlang::enquo(var)

  x <- data %>%
    dplyr::pull(!! var)

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


  lamisc::sktest(data = transformed_x,
                 "cubic",
                 "square",
                 "identity",
                 "square_root",
                 "log",
                 "inv_square_root",
                 "inverse",
                 "inv_square",
                 "inv_cubic") %>%
    dplyr::select(transformation = var,
                  adj_chi2,
                  p_value) %>%
    # mutate(p_value = lamisc::fmt_pvl(x = p_value),
    #        p_value = stringr::str_pad(p_value,
    #                                   width = max(nchar(p_value)),
    #                                   side = "left",
    #                                   pad = " ")) %>%
    dplyr::mutate(p_value = lamisc::fmt_pvl(x = p_value))
}

