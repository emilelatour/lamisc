#' @title
#' Force a square version of base::table()
#'
#' @description
#' `base::table()` will not return a square table for a confusion matrix if a
#' fator level is not present in one of the variables. This will return a square
#' table.
#'
#' @param x "X" variable; counts appear in the rows of the table
#' @param y "Y" variable; counts appear in the columns of the table
#'
#' @references
#' https://stackoverflow.com/questions/5558745/force-table-to-include-all-factors-from-both-arrays-in-r
#'
#' @return A table
#' @export
#'
#' @examples
#' library(tibble)
#'
#' #### Example 1 --------------------------------
#'
#' df <- tibble::tribble(
#'   ~a, ~b, ~c,
#'   1L, 1L, 1L,
#'   0L, 1L, 1L,
#'   1L, 1L, 0L,
#'   0L, 1L, 0L,
#'   1L, 1L, 1L,
#'   0L, 1L, 1L,
#'   1L, 1L, 0L
#' )
#'
#' table(df$a, df$b)
#' square_table(df$a, df$b)
#'
#'
#' #### Example 2 --------------------------------
#'
#' set.seed(8675309)
#'
#' df2 <- tibble::tibble(
#'   x = factor(sample(0:9, 100, TRUE)),
#'   y = factor(sample(3:7, 100, TRUE))
#' )
#'
#' with(df2, table(x, y))
#'
#' with(df2, square_table(x, y))
#' with(df2, square_table(y, x))
#'
#'
#' #### Example 3 --------------------------------
#'
#' set.seed(8675309)
#'
#' df3 <- tibble::tibble(
#'   x = factor(sample(0:2, 100, TRUE)),
#'   y = factor(sample(3:5, 100, TRUE))
#' )
#'
#' with(df3, table(x, y))
#'
#' with(df3, square_table(x, y))
#' with(df3, square_table(y, x))

square_table <- function(x, y) {

  x <- factor(x)
  y <- factor(y)

  common_lvls <- sort(unique(c(levels(x), levels(y))))

  x <- factor(x, levels = common_lvls)
  y <- factor(y, levels = common_lvls)

  table(x, y)

}

