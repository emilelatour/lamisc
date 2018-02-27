
#### my_colors --------------------------------

# Make a list of my_colors to save as a data object. This will get used by the
# user function my_pal()

my_colors <- list(
  sb_deep = c(
    "#4C72B0", "#55A868", "#C44E52",
    "#8172B2", "#CCB974", "#64B5CD"
  ),
  sb_muted = c(
    "#4878CF", "#6ACC65", "#D65F5F",
    "#B47CC7", "#C4AD66", "#77BEDB"
  ),
  sb_pastel = c(
    "#92C6FF", "#97F0AA", "#FF9F9A",
    "#D0BBFF", "#FFFEA3", "#B0E0E6"
  ),
  sb_bright = c(
    "#003FFF", "#03ED3A", "#E8000B",
    "#8A2BE2", "#FFC400", "#00D7FF"
  ),
  sb_dark = c(
    "#001C7F", "#017517", "#8C0900",
    "#7600A1", "#B8860B", "#006374"
  ),
  sb_colorblind = c(
    "#0072B2", "#009E73", "#D55E00",
    "#CC79A7", "#F0E442", "#56B4E9"
  ),
  lancet_colors = c(
    "#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F",
    "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919"
  ),
  jco_colors = c(
    "#0073C2", "#EFC000", "#868686", "#CD534C", "#7AA6DC",
    "#003C67", "#8F7700", "#3B3B3B", "#A73030", "#4A6990"
  ),
  basic_colors =
    c(
      "steelblue", "dodgerblue", "firebrick", "goldenrod", "darkblue",
      "lightseagreen", "palegreen", "azure3"
    ),
  qualitative_colors =
    c(
      "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33",
      "#a65628", "#f781bf"
    ),
  tol_colors =
    c(
      "#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77",
      "#CC6677", "#882255", "#AA4499"
    )
)


## Then save it in data folder ----------------

# save(my_colors, file = here::here("data", "my_colors.RData"))


#### Test it out a little --------------------------------

# load(here::here("data", "my-colors.RData"))
# library(magrittr)
# my_colors[["sb_deep"]] %>%
#   pie(rep(1, length(.)),
#       labels = sprintf("%d (%s)", seq_along(.), .),
#       col = .)
#
# my_colors[["tol_colors"]] %>%
#   pie(rep(1, length(.)),
#       labels = sprintf("%d (%s)", seq_along(.), .),
#       col = .)

