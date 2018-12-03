

#' Convert from a table or data frame of counts to data frame of cases.
#'
#' Takes a data frame or a table object in R with counts and converts it to a
#' data frame with the cases.
#'
#' @references
#' \href{http://www.cookbook-r.com/Manipulating_data/Converting_between_data_frames_and_contingency_tables/}{cookbook-r.com, Converting_between_data_frames_and_contingency_tables}
#' \href{http://www.cookbook-r.com/Manipulating_data/Converting_between_data_frames_and_contingency_tables/#countstocases-function}{cookbook-r.com countstocases function}
#'
#' @param input_table A table object
#' @param countcol the name of the column containing the counts
#'   (default = "Freq")
#'
#' @import vcd
#' @import tibble
#'
#' @return A data frame
#' @export
#'
#' @examples
#' #### Example 1 --------------------------------
#' library(vcd)
#' class(SexualFun)
#' counts_to_cases(input_table = SexualFun,
#'                 countcol = "Freq")
#'
#' #### Example 2 --------------------------------
#' phs <- matrix(c(189, 10845, 104, 10933), byrow = TRUE, ncol = 2)
#' dimnames(phs) <- list(Group = c("Placebo", "Aspirin"), MI = c("Yes", "No"))
#' phs
#' class(phs)
#' counts_to_cases(input_table = phs,
#'                 countcol = "Freq")
#'
#' #### Example 3 --------------------------------
#' # This last example uses data from a frequency tablethat might be entered into
#' # SAS. This serves as a helpful reminder to myself.
#' library(dplyr)
#' library(tibble)
#' library(irr)  # For the kappa2 function.
#'
#' rate <- tibble::tribble(
#'   ~rater1, ~rater2, ~freq,
#'      "Lo",    "Lo",     0,
#'      "Lo",   "Med",     0,
#'      "Lo",    "Hi",     0,
#'     "Med",    "Lo",     5,
#'     "Med",   "Med",    16,
#'     "Med",    "Hi",     3,
#'      "Hi",    "Lo",     8,
#'      "Hi",   "Med",    12,
#'      "Hi",    "Hi",    28
#'   )
#'
#' rate <- tibble::tribble(
#'   ~rater1, ~rater2, ~freq,
#'      "Lo",    "Lo",     0,
#'      "Lo",   "Med",     0,
#'      "Lo",    "Hi",     0,
#'     "Med",    "Lo",     5,
#'     "Med",   "Med",    16,
#'     "Med",    "Hi",     3,
#'      "Hi",    "Lo",     8,
#'      "Hi",   "Med",    12,
#'      "Hi",    "Hi",    28
#'   )
#'
#' (rate <- xtabs(freq ~ rater1 + rater2, data = rate))
#'
#' (rate <- lamisc::counts_to_cases(input_table = rate) %>%
#'     mutate_all(.tbl = .,
#'              .funs = funs(factor(.,
#'                                  levels = c("Lo", "Med", "Hi"),
#'                                  labels = c("Lo", "Med", "Hi")))))
#'
#' irr::kappa2(rate)
#'
counts_to_cases <- function(input_table, countcol = "Freq") {

  if (!(inherits(input_table, "matrix") | inherits(input_table, "table")))
    stop("Input must be a table or a matrix.")

  x <- as.data.frame(as.table(input_table))

  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])

  # Drop count column
  x[[countcol]] <- NULL

  # Get the rows from x
  tibble::as_tibble(x[idx, ])
}


#### Original function --------------------------------
# From http://www.cookbook-r.com/Manipulating_data/Converting_between_data_frames_and_contingency_tables/#countstocases-function

# Convert from data frame of counts to data frame of cases.
# `countcol` is the name of the column containing the counts
# countsToCases <- function(x, countcol = "Freq") {
#   # Get the row indices to pull from x
#   idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
#
#   # Drop count column
#   x[[countcol]] <- NULL
#
#   # Get the rows from x
#   x[idx, ]
# }
