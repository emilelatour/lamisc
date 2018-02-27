

#' plot_colors and pie_palette
#'
#' Various user defined palettes to use. And the function pie_palette() which
#' takes a palette as an argument and returns a pie chart with labels; handy
#' when you only want to pick specific colors from a palette and you don't know
#' what they are yet.
#'
#' @param palette The palette that you want to visualize
#'
#' @export
#'
#' @examples
#' library(lamisc)
#' pie_palette(la_palette("sb_deep"))

#### plot_colors --------------------------------

# These are some custom palettes that I like to use. References and links are
# given where appropriate.


#### pie_palette() --------------------------------

# Function to print pie chart to preview what a palette looks like and to see
# the color codes.

pie_palette <- function(palette) {

  palette %>%
    pie(rep(1, length(.)),
        labels = sprintf("%d (%s)", seq_along(.), .),
        col = .)

}


