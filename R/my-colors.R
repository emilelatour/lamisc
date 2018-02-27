

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
#' pie_palette(sb_deep)

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

#### Colors --------------------------------

## plot colors #1 ---------------

plot_colors <- c("steelblue",
                 "dodgerblue",
                 "firebrick",
                 "goldenrod",
                 "darkblue",
                 "lightseagreen",
                 "palegreen",
                 "azure3")


## plot colors #2 ---------------

qualitative_colors <- c("#e41a1c",
                        "#377eb8",
                        "#4daf4a",
                        "#984ea3",
                        "#ff7f00",
                        "#ffff33",
                        "#a65628",
                        "#f781bf")


## Paul Tol Knock Off ---------------

# https://personal.sron.nl/~pault/

tol_colors <- c("#332288",
                "#88CCEE",
                "#44AA99",
                "#117733",
                "#999933",
                "#DDCC77",
                "#CC6677",
                "#882255",
                "#AA4499")


## Seaborn color palettes----------------

# these are all 6 seaborn color palettes
# from https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py

sb_deep <- c("#4C72B0",
             "#55A868",
             "#C44E52",
             "#8172B2",
             "#CCB974",
             "#64B5CD")

sb_muted <- c("#4878CF",
              "#6ACC65",
              "#D65F5F",
              "#B47CC7",
              "#C4AD66",
              "#77BEDB")

sb_pastel <- c("#92C6FF",
               "#97F0AA",
               "#FF9F9A",
               "#D0BBFF",
               "#FFFEA3",
               "#B0E0E6")

sb_bright <- c("#003FFF",
               "#03ED3A",
               "#E8000B",
               "#8A2BE2",
               "#FFC400",
               "#00D7FF")

sb_dark <- c("#001C7F",
             "#017517",
             "#8C0900",
             "#7600A1",
             "#B8860B",
             "#006374")

sb_colorblind <- c("#0072B2",
                   "#009E73",
                   "#D55E00",
                   "#CC79A7",
                   "#F0E442",
                   "#56B4E9")
