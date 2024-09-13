
#' @title
#' Ladder-of-powers histograms
#'
#' @description
#' Explore Tukey's ladder of powers graphically using histograms to identify a
#' transform that converts `var`into a normally distributed variable.
#'
#' Density is shown on the y-axis. The heights of the bars of the histogram
#' are scaled so that the sum of their areas is equal to 1.0. Normal curves are
#' plotted using the mean and standard deviation of the transformations, not the
#' kernel density of the histograms.
#'
#' This is intended to replicate the `gladder` command in Stata. As per Stata's
#' approach, the number of bins is set to the `min(sqrt(n), 10 * log10(n))`,
#' rounded to the closest integer, where n is number of observations. NAs
#' are omitted by default.
#'
#' @references
#' https://www.stata.com/manuals/rladder.pdf
#'
#' @param data A tibble or data frame.
#' @param var A numeric vector of data values.
#' @param fill Fill color for the histograms (default = "cyan4").
#' @param line_color Color for the normal curves (default = "black").
#' @param line_type Line type for the normal curves (default = "solid").
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
#' @importFrom stats dnorm
#' @importFrom tibble tibble
#'
#' @return
#' A tibble.
#'
#' @export
#'
#' @examples
#'
#' ggladder(data = auto,
#'          var = mpg)
#'
#' ggladder(data = auto,
#'          var = mpg,
#'          fill = "pink")
#'
#' # To adjust the theme of all of the patches using patchwork package and an &
#' # instead of +
#' ggladder(data = auto,
#'          var = mpg) &
#'   ggplot2::theme_classic()
#'

ggladder <- function(data, var, fill = "cyan4",
                     line_color = "black",
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


  hist_list <- purrr::map2(.x = names(transformed_x),
                           .y = c("Cubic",
                                  "Square",
                                  "Identity",
                                  "Square root",
                                  "Log",
                                  "1 / Square root",
                                  "Inverse",
                                  "1 / Square",
                                  "1 / Cubic"),
                           .f = ~ make_histos(df = transformed_x,
                                              x = .x,
                                              x_title = .y,
                                              fill = fill,
                                              line_color = line_color,
                                              line_type = line_type))


  hist_combo <- patchwork::wrap_plots(hist_list)

  # p4 <- ggplot(data.frame(l = "Density", x = 1, y = 1)) +
  #       geom_text(aes(x, y, label = l), angle = 90) +
  #       theme_void() +
  #       coord_cartesian(clip = "off")
  #
  # hist_combo <- p4 + hist_combo + plot_layout(widths = c(1, 25))

  hist_combo +
    patchwork::plot_annotation(title = "Density histograms by transformation",
                               subtitle = var_name)

}


#' @title Internal function - Make the histograms
#'
#' @description
#' This function generates histograms for a given numeric vector in a data frame.
#'
#' @name make_histos
#'
#' @param df A data frame or tibble.
#' @param x A (non-empty) numeric vector of data values.
#' @param x_title The title for the plot
#' @param fill Color for the histograms
#' @param line_color Color for the line
#' @param line_type Line type
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 stat_function
#' @importFrom rlang sym
#'
#' @keywords internal

utils::globalVariables(c("..density.."))

make_histos <- function(df, x, x_title,
                        fill = "cyan4", line_color = "black",
                        line_type = "solid") {

  # Fix no visible binding for global variable
  sqrt_n <- ten_log_10 <- NULL

  mean_sd <- df %>%
    dplyr::summarise(mean = mean(!! rlang::sym(x), na.rm = TRUE),
                     sd = sd(!! rlang::sym(x), na.rm = TRUE))

  n_bins <- df %>%
    # summarise(n = sum(!is.na(unique(!! rlang::sym(x))))) %>%
    dplyr::summarise(n = sum(!is.na(!! rlang::sym(x)))) %>%
    dplyr::mutate(sqrt_n = round(sqrt(n)),
                  ten_log_10 = round(10 * log10(n))) %>%
    dplyr::summarise(n_bins = min(sqrt_n, ten_log_10))


  ggplot(data = df,
         aes(x = !! rlang::sym(x))) +
    geom_histogram(aes(y = ..density..),
                   # binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)),
                   bins = n_bins$n_bins,
                   fill =  fill,
                   alpha = 0.8,
                   color = "white") +
    # geom_density(color = "darkorchid") +
    stat_function(fun = dnorm,
                  args = list(mean = mean_sd$mean,
                              sd = mean_sd$sd),
                  size = 1.0,
                  colour = line_color,
                  linetype = line_type) +
    labs(x = NULL,
         y = NULL,
         title = x_title)

}
