

#' Convert from a table or data frame of counts to data frame of cases.
#'
#' Takes a data frame or a table object in R with counts and converts it to a
#' data frame with the cases. Source:
#' \url{http://www.cookbook-r.com/Manipulating_data/Converting_between_data_frames_and_contingency_tables/#countstocases-function}
#'
#' @param input_table A table object
#' @param countcol the name of the column containing the counts
#'   (default = "Freq")
#'
#' @import vcd
#'
#' @return A data frame
#' @export
#'
#' @examples
#' library(vcd)
#' class(SexualFun)
#' counts_to_cases(input_table = SexualFun,
#'                 countcol = "Freq")
#'
#' phs <- matrix(c(189, 10845, 104, 10933), byrow = TRUE, ncol = 2)
#' dimnames(phs) <- list(Group = c("Placebo", "Aspirin"), MI = c("Yes", "No"))
#' phs
#' class(phs)
#' counts_to_cases(input_table = phs,
#'                 countcol = "Freq")


counts_to_cases <- function(input_table, countcol = "Freq") {

  if (!(inherits(input_table, "matrix") | inherits(input_table, "table")))
    stop("Input must be a table or a matrix.")

  x <- as.data.frame(as.table(input_table))

  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])

  # Drop count column
  x[[countcol]] <- NULL

  # Get the rows from x
  x[idx, ]
}

