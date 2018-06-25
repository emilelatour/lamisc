# example using similar idea to duplicated.data.frame
#' @title
#' Count the number of duplicate rows in a data frame
#'
#' @description
#' Given a data frame, this will retun a data frame of the duplicate rows with a
#' column for the number of times that it appears in the data.
#'
#' Very similar and not as preferred to the `get_dupes` function in Sam Firke's
#' `janitor` package. I did borrow some code from that one to deal with cases
#' when variable are specified and when they are not (variables are arguments to
#' `...`).
#'
#' @references
#'
#' \url{https://stackoverflow.com/questions/18201074/find-how-many-times-duplicated-rows-repeat-in-r-data-frame}
#'
#' \url{https://cran.r-project.org/web/packages/janitor/janitor.pdf}
#'
#' \url{https://github.com/sfirke/janitor/blob/master/R/get_dupes.R}
#'
#' @import dplyr
#' @import rlang
#' @param data A data frame or tibble
#' @param ... Unquoted variable names to search for duplicates.
#'
#' @return
#' Returns a data.frame (actually a \code{tbl_df}) with the full records where
#' the specified variables have duplicated values, as well as a variable
#' \code{dupe_count} showing the number of rows sharing that combination of
#' duplicated values.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' (DF <- data.frame(replicate(sequence(1:3), n = 4)))
#' count_duplicates(DF)
#' count_duplicates(DF, X2, X3)
#' # Pipeable also
#' DF %>%
#'   count_duplicates(.)

count_duplicates <- function(data, ...) {

  # unquoted names for NSE calls, need quoted names separately for messages +
  # warnings
  uq_names <- as.list(substitute(list(...)))[-1L]
  df_name <- deparse(substitute(data))

  if (length(uq_names) == 0) {
    # if called on an entire data.frame with no specified variable names
    var_names <- names(data)
    nms <- rlang::syms(var_names)
    message("No variable names specified - using all columns.\n")
  } else {
    # nms <- rlang::quos(X2:X3) %>%
    #   rlang::quos_auto_name()
    # var_names <- names(nms)
    nms <- rlang::enquos(...)
  }

  df <- data %>%
    dplyr::select(!!! nms)

  x <- do.call('paste', c(df, sep = '\r'))
  ox <- order(x)
  rl <- rle(x[ox])
  cbind(df[ox[cumsum(rl$lengths)], , drop = FALSE], dupe_count = rl$lengths)

}


