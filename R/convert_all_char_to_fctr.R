
#' @title
#' Converts all character vectors to factors
#'
#' @description
#' This function takes a tbl_df and returns same with any character column
#' converted to a factor.
#'
#' @param data Tibble or data frame
#'
#' @return A tibble
#' @export
#'
#' @examples
#' library(dplyr)
#' dplyr::glimpse(starwars)
#' convert_all_char_to_fctr(data = starwars)
#'
convert_all_char_to_fctr <- function(data) {

  char_cols <-
    names(data)[sapply(data, function(x) {class(x) == "character" })]

  data %>%
    mutate_at(.vars = vars(char_cols),
              .funs = funs(as.factor))

}

