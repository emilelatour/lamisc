#' Complete list of palettes
#'
#' Use \code{\link{my_colors}} to construct palettes of desired length.
#'
#' @name my_colors
#' @docType data
NULL

#' My preferred pallettes generator
#'
#' A few of my favorite and most used (discrete) palettes. Some are just base
#' R colors put into a vector. Others come from the follwoing sources:
#' https://github.com/mwaskom/seaborn/blob/master/seaborn/palettes.py
#' https://personal.sron.nl/~pault/
#'
#' Adapted from [karthik/wesanderson](https://github.com/karthik/wesanderson)
#' and [dill/beyonce](https://github.com/dill/beyonce)
#'
#' @param name Name of desired palette. Choices are:
#'   \code{sb_deep}, \code{sb_muted},  \code{sb_pastel}, \code{sb_bright},
#'   \code{sb_dark},  \code{sb_colorblind}, \code{lancet_colors},
#'   \code{jco_colors}, \code{basic_colors}, \code{qualitative_colors},
#'   \code{tol_colors}
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colours.
#' @return A vector of colours.
#' @export
#' @keywords colours
#' @examples
#' library(lamisc)
#' library(tidyverse)
#' my_palette("tol_colors")
#' my_palette("tol_colors") %>%
#'   lamisc::view_palette()
#' ggplot(data = mtcars, aes(x = factor(cyl), fill = factor(vs))) +
#'   geom_bar(stat = "count") +
#'   scale_fill_manual(values = my_palette("tol_colors"))
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point(size = 3) +
#'   scale_color_manual(values = my_palette("lancet_colors")) +
#'   theme_minimal()
#'
#'
my_palette <- function(name, type = c("discrete", "continuous")) {

  type <- match.arg(type)

  data("my_colors")

  pal <- my_colors[[name]]

  if (is.null(pal))
    stop("Palette not found.")

  n <- length(pal)

  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colours greater than what palette can offer")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

