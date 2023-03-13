
#' @title
#' Ladder-of-powers quantile-normal plots
#'
#' @description
#' Explore Tukey's ladder of powers graphically using quantile plots to identify
#' a transform that converts `var`into a normally distributed variable.
#'
#' This is intended to replicate the `qladder` command in Stata.
#'
#' @references
#' https://www.stata.com/manuals/rladder.pdf
#'
#' @param data A tibble or data frame.
#' @param var A numeric vector of data values.
#' @param point_color Color for the quantile points (default = "cyan4").
#' @param line_color Color for the normal line (default = "black").
#' @param line_type Line type for the normal line (default = "solid").
#'
#' @import ggplot2
#' @import patchwork
#'
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr summarise
#' @importFrom purrr map2
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom tibble tibble
#'
#' @return
#' A tibble.
#'
#' @export
#'
#' @examples
#'
#' qqladder(data = auto,
#'          var = mpg)
#'
#' qqladder(data = auto,
#'          var = mpg,
#'          point_color = "purple")
#'
#' qqladder(data = auto,
#'          var = weight,
#'          point_color = "purple",
#'          line_type = "dashed")
#'
#'
#' # To adjust the theme of all of the patches using patchwork package and an &
#' # instead of +
#' qqladder(data = auto,
#'          var = weight) &
#'   ggplot2::theme_classic()
#'
#'
qqladder <- function(data, var,
                     point_color = "cyan4", line_color = "black",
                     line_type = "solid") {

  # Fix no visible binding for global variable
  inv_square_root <- NULL
  inverse <- NULL
  inv_square <- NULL
  inv_cubic <- NULL


  var <- rlang::enquo(var)
  var_name <- rlang::quo_name(var)

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
    inv_cubic = 1 / (x ^ 3)) %>%
    dplyr::mutate(dplyr::across(.cols = c(inv_square_root,
                                          inverse,
                                          inv_square,
                                          inv_cubic),
                                .fns = ~ -1 * .))


  qq_list <- purrr::map2(.x = names(transformed_x),
                         .y = c("Cubic",
                                "Square",
                                "Identity",
                                "Square root",
                                "Log",
                                "1 / Square root",
                                "Inverse",
                                "1 / Square",
                                "1 / Cubic"),
                         .f = ~ make_qq(df = transformed_x,
                                        x = .x,
                                        x_title = .y,
                                        point_color = point_color,
                                        line_color = line_color,
                                        line_type = line_type))


  combo <- patchwork::wrap_plots(qq_list)

  combo +
    patchwork::plot_annotation(title = "Quantile-Normal plots by transformation",
                               subtitle = var_name)

}


#### Helper functions --------------------------------

make_qq <- function(df, x, x_title,
                    point_color = "cyan4", line_color = "black",
                    line_type = "solid") {

  ggplot(data = df,
         aes(sample = !! rlang::sym(x))) +
    geom_qq_line(# size = 1.0,
      color = line_color,
      linetype = line_type) +
    geom_qq(color =  point_color,
            alpha = 0.8) +
    labs(x = NULL,
         y = NULL,
         title = x_title) +
    # theme(axis.title.x=element_blank(),
    #     axis.text.x=element_blank(),
    #     axis.ticks.x=element_blank()) +
    NULL

}

