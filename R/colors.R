#' Complete list of palettes
#'
#' Adapted from karthik/wesanderson
#'
#'
#' @export
la_palettes <- list(
  basic_colors =
    c("steelblue", "dodgerblue", "firebrick", "goldenrod", "darkblue",
      "lightseagreen", "palegreen", "azure3"),
  qualitative_colors =
    c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33",
      "#a65628", "#f781bf"),
  tol_colors =
    c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77",
      "#CC6677", "#882255", "#AA4499"),
  sb_deep =
    c("#4C72B0", "#55A868", "#C44E52", "#8172B2", "#CCB974", "#64B5CD"),
  sb_muted =
    c("#4878CF", "#6ACC65", "#D65F5F", "#B47CC7", "#C4AD66", "#77BEDB"),
  sb_pastel =
    c("#92C6FF", "#97F0AA", "#FF9F9A", "#D0BBFF", "#FFFEA3", "#B0E0E6"),
  sb_bright =
    c("#003FFF", "#03ED3A", "#E8000B", "#8A2BE2", "#FFC400", "#00D7FF"),
  sb_dark =
    c("#001C7F", "#017517", "#8C0900", "#7600A1", "#B8860B", "#006374"),
  sb_colorblind =
    c("#0072B2", "#009E73", "#D55E00", "#CC79A7", "#F0E442", "#56B4E9")
)

#' A few of my favorite and most used (discrete) palettes. Some are just base
#' R colors put into a vector. Others come from the follwoing sources:
#' https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py
#' https://personal.sron.nl/~pault/
#'
#'
#' These are a handful of color palettes from Wes Anderson movies.
#'
#' @param name Name of desired palette. Choices are:
#'   \code{basic_colors}, \code{qualitative_colors},  \code{tol_colors},
#'   \code{sb_deep}, \code{sb_muted},  \code{sb_pastel}, \code{sb_bright},
#'   \code{sb_dark},  \code{sb_colorblind}
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#' library(lamisc)
#' la_palette("sb_deep")
#' la_palette("tol_colors")
#' la_palette("qualitative_colors")
#'
#'
la_palette <- function(name) {

  pal <- la_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")

  n <- length(pal)

  out <- pal[1:n]

  structure(out, class = "palette", name = name)
}

