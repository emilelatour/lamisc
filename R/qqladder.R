
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
#' library(dplyr)
#' library(tibble)
#'
#' auto <- tibble::tribble(
#'   ~make, ~price, ~mpg, ~rep78, ~headroom, ~trunk, ~weight, ~length, ~turn, ~displacement, ~gear_ratio,   ~foreign,
#'   "AMC Concord",  4099L,  22L,     3L,       2.5,    11L,   2930L,    186L,   40L,          121L,        3.58, "Domestic",
#'   "AMC Pacer",  4749L,  17L,     3L,         3,    11L,   3350L,    173L,   40L,          258L,        2.53, "Domestic",
#'   "AMC Spirit",  3799L,  22L,     NA,         3,    12L,   2640L,    168L,   35L,          121L,        3.08, "Domestic",
#'   "Buick Century",  4816L,  20L,     3L,       4.5,    16L,   3250L,    196L,   40L,          196L,        2.93, "Domestic",
#'   "Buick Electra",  7827L,  15L,     4L,         4,    20L,   4080L,    222L,   43L,          350L,        2.41, "Domestic",
#'   "Buick LeSabre",  5788L,  18L,     3L,         4,    21L,   3670L,    218L,   43L,          231L,        2.73, "Domestic",
#'   "Buick Opel",  4453L,  26L,     NA,         3,    10L,   2230L,    170L,   34L,          304L,        2.87, "Domestic",
#'   "Buick Regal",  5189L,  20L,     3L,         2,    16L,   3280L,    200L,   42L,          196L,        2.93, "Domestic",
#'   "Buick Riviera", 10372L,  16L,     3L,       3.5,    17L,   3880L,    207L,   43L,          231L,        2.93, "Domestic",
#'   "Buick Skylark",  4082L,  19L,     3L,       3.5,    13L,   3400L,    200L,   42L,          231L,        3.08, "Domestic",
#'   "Cad. Deville", 11385L,  14L,     3L,         4,    20L,   4330L,    221L,   44L,          425L,        2.28, "Domestic",
#'   "Cad. Eldorado", 14500L,  14L,     2L,       3.5,    16L,   3900L,    204L,   43L,          350L,        2.19, "Domestic",
#'   "Cad. Seville", 15906L,  21L,     3L,         3,    13L,   4290L,    204L,   45L,          350L,        2.24, "Domestic",
#'   "Chev. Chevette",  3299L,  29L,     3L,       2.5,     9L,   2110L,    163L,   34L,          231L,        2.93, "Domestic",
#'   "Chev. Impala",  5705L,  16L,     4L,         4,    20L,   3690L,    212L,   43L,          250L,        2.56, "Domestic",
#'   "Chev. Malibu",  4504L,  22L,     3L,       3.5,    17L,   3180L,    193L,   31L,          200L,        2.73, "Domestic",
#'   "Chev. Monte Carlo",  5104L,  22L,     2L,         2,    16L,   3220L,    200L,   41L,          200L,        2.73, "Domestic",
#'   "Chev. Monza",  3667L,  24L,     2L,         2,     7L,   2750L,    179L,   40L,          151L,        2.73, "Domestic",
#'   "Chev. Nova",  3955L,  19L,     3L,       3.5,    13L,   3430L,    197L,   43L,          250L,        2.56, "Domestic",
#'   "Dodge Colt",  3984L,  30L,     5L,         2,     8L,   2120L,    163L,   35L,           98L,        3.54, "Domestic",
#'   "Dodge Diplomat",  4010L,  18L,     2L,         4,    17L,   3600L,    206L,   46L,          318L,        2.47, "Domestic",
#'   "Dodge Magnum",  5886L,  16L,     2L,         4,    17L,   3600L,    206L,   46L,          318L,        2.47, "Domestic",
#'   "Dodge St. Regis",  6342L,  17L,     2L,       4.5,    21L,   3740L,    220L,   46L,          225L,        2.94, "Domestic",
#'   "Ford Fiesta",  4389L,  28L,     4L,       1.5,     9L,   1800L,    147L,   33L,           98L,        3.15, "Domestic",
#'   "Ford Mustang",  4187L,  21L,     3L,         2,    10L,   2650L,    179L,   43L,          140L,        3.08, "Domestic",
#'   "Linc. Continental", 11497L,  12L,     3L,       3.5,    22L,   4840L,    233L,   51L,          400L,        2.47, "Domestic",
#'   "Linc. Mark V", 13594L,  12L,     3L,       2.5,    18L,   4720L,    230L,   48L,          400L,        2.47, "Domestic",
#'   "Linc. Versailles", 13466L,  14L,     3L,       3.5,    15L,   3830L,    201L,   41L,          302L,        2.47, "Domestic",
#'   "Merc. Bobcat",  3829L,  22L,     4L,         3,     9L,   2580L,    169L,   39L,          140L,        2.73, "Domestic",
#'   "Merc. Cougar",  5379L,  14L,     4L,       3.5,    16L,   4060L,    221L,   48L,          302L,        2.75, "Domestic",
#'   "Merc. Marquis",  6165L,  15L,     3L,       3.5,    23L,   3720L,    212L,   44L,          302L,        2.26, "Domestic",
#'   "Merc. Monarch",  4516L,  18L,     3L,         3,    15L,   3370L,    198L,   41L,          250L,        2.43, "Domestic",
#'   "Merc. XR-7",  6303L,  14L,     4L,         3,    16L,   4130L,    217L,   45L,          302L,        2.75, "Domestic",
#'   "Merc. Zephyr",  3291L,  20L,     3L,       3.5,    17L,   2830L,    195L,   43L,          140L,        3.08, "Domestic",
#'   "Olds 98",  8814L,  21L,     4L,         4,    20L,   4060L,    220L,   43L,          350L,        2.41, "Domestic",
#'   "Olds Cutl Supr",  5172L,  19L,     3L,         2,    16L,   3310L,    198L,   42L,          231L,        2.93, "Domestic",
#'   "Olds Cutlass",  4733L,  19L,     3L,       4.5,    16L,   3300L,    198L,   42L,          231L,        2.93, "Domestic",
#'   "Olds Delta 88",  4890L,  18L,     4L,         4,    20L,   3690L,    218L,   42L,          231L,        2.73, "Domestic",
#'   "Olds Omega",  4181L,  19L,     3L,       4.5,    14L,   3370L,    200L,   43L,          231L,        3.08, "Domestic",
#'   "Olds Starfire",  4195L,  24L,     1L,         2,    10L,   2730L,    180L,   40L,          151L,        2.73, "Domestic",
#'   "Olds Toronado", 10371L,  16L,     3L,       3.5,    17L,   4030L,    206L,   43L,          350L,        2.41, "Domestic",
#'   "Plym. Arrow",  4647L,  28L,     3L,         2,    11L,   3260L,    170L,   37L,          156L,        3.05, "Domestic",
#'   "Plym. Champ",  4425L,  34L,     5L,       2.5,    11L,   1800L,    157L,   37L,           86L,        2.97, "Domestic",
#'   "Plym. Horizon",  4482L,  25L,     3L,         4,    17L,   2200L,    165L,   36L,          105L,        3.37, "Domestic",
#'   "Plym. Sapporo",  6486L,  26L,     NA,       1.5,     8L,   2520L,    182L,   38L,          119L,        3.54, "Domestic",
#'   "Plym. Volare",  4060L,  18L,     2L,         5,    16L,   3330L,    201L,   44L,          225L,        3.23, "Domestic",
#'   "Pont. Catalina",  5798L,  18L,     4L,         4,    20L,   3700L,    214L,   42L,          231L,        2.73, "Domestic",
#'   "Pont. Firebird",  4934L,  18L,     1L,       1.5,     7L,   3470L,    198L,   42L,          231L,        3.08, "Domestic",
#'   "Pont. Grand Prix",  5222L,  19L,     3L,         2,    16L,   3210L,    201L,   45L,          231L,        2.93, "Domestic",
#'   "Pont. Le Mans",  4723L,  19L,     3L,       3.5,    17L,   3200L,    199L,   40L,          231L,        2.93, "Domestic",
#'   "Pont. Phoenix",  4424L,  19L,     NA,       3.5,    13L,   3420L,    203L,   43L,          231L,        3.08, "Domestic",
#'   "Pont. Sunbird",  4172L,  24L,     2L,         2,     7L,   2690L,    179L,   41L,          151L,        2.73, "Domestic",
#'   "Audi 5000",  9690L,  17L,     5L,         3,    15L,   2830L,    189L,   37L,          131L,         3.2,  "Foreign",
#'   "Audi Fox",  6295L,  23L,     3L,       2.5,    11L,   2070L,    174L,   36L,           97L,         3.7,  "Foreign",
#'   "BMW 320i",  9735L,  25L,     4L,       2.5,    12L,   2650L,    177L,   34L,          121L,        3.64,  "Foreign",
#'   "Datsun 200",  6229L,  23L,     4L,       1.5,     6L,   2370L,    170L,   35L,          119L,        3.89,  "Foreign",
#'   "Datsun 210",  4589L,  35L,     5L,         2,     8L,   2020L,    165L,   32L,           85L,         3.7,  "Foreign",
#'   "Datsun 510",  5079L,  24L,     4L,       2.5,     8L,   2280L,    170L,   34L,          119L,        3.54,  "Foreign",
#'   "Datsun 810",  8129L,  21L,     4L,       2.5,     8L,   2750L,    184L,   38L,          146L,        3.55,  "Foreign",
#'   "Fiat Strada",  4296L,  21L,     3L,       2.5,    16L,   2130L,    161L,   36L,          105L,        3.37,  "Foreign",
#'   "Honda Accord",  5799L,  25L,     5L,         3,    10L,   2240L,    172L,   36L,          107L,        3.05,  "Foreign",
#'   "Honda Civic",  4499L,  28L,     4L,       2.5,     5L,   1760L,    149L,   34L,           91L,         3.3,  "Foreign",
#'   "Mazda GLC",  3995L,  30L,     4L,       3.5,    11L,   1980L,    154L,   33L,           86L,        3.73,  "Foreign",
#'   "Peugeot 604", 12990L,  14L,     NA,       3.5,    14L,   3420L,    192L,   38L,          163L,        3.58,  "Foreign",
#'   "Renault Le Car",  3895L,  26L,     3L,         3,    10L,   1830L,    142L,   34L,           79L,        3.72,  "Foreign",
#'   "Subaru",  3798L,  35L,     5L,       2.5,    11L,   2050L,    164L,   36L,           97L,        3.81,  "Foreign",
#'   "Toyota Celica",  5899L,  18L,     5L,       2.5,    14L,   2410L,    174L,   36L,          134L,        3.06,  "Foreign",
#'   "Toyota Corolla",  3748L,  31L,     5L,         3,     9L,   2200L,    165L,   35L,           97L,        3.21,  "Foreign",
#'   "Toyota Corona",  5719L,  18L,     5L,         2,    11L,   2670L,    175L,   36L,          134L,        3.05,  "Foreign",
#'   "VW Dasher",  7140L,  23L,     4L,       2.5,    12L,   2160L,    172L,   36L,           97L,        3.74,  "Foreign",
#'   "VW Diesel",  5397L,  41L,     5L,         3,    15L,   2040L,    155L,   35L,           90L,        3.78,  "Foreign",
#'   "VW Rabbit",  4697L,  25L,     4L,         3,    15L,   1930L,    155L,   35L,           89L,        3.78,  "Foreign",
#'   "VW Scirocco",  6850L,  25L,     4L,         2,    16L,   1990L,    156L,   36L,           97L,        3.78,  "Foreign",
#'   "Volvo 260", 11995L,  17L,     5L,       2.5,    14L,   3170L,    193L,   37L,          163L,        2.98,  "Foreign"
#' )
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
