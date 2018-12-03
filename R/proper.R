#' @title
#' Capitalize the first letter (option: in each word)
#'
#' @description
#' This is a riff on the Excel PROPER function which capitalizes the words in a
#' given text string. Numbers and punctuation are not affected.
#'
#' @param x Character; the text that should be converted to proper case.
#' @param just_first_word Logical; If TRUE (default) only the first letter of
#'   the first word of the text will be capitalized, else if FALSE then the
#'   first letter of each word will be capitalized.
#'
#' @return A chacter string
#' @export
#'
#' @examples
#' sample <- c("zachary taylor", "To be or not to be", "san diego, CA")
#' proper(sample)
#' proper(sample, just_first_word = FALSE)
#'
#' another_sample <- c("this is a TITLE", "2-way street", "76BudGet")
#' proper(another_sample)
#' proper(another_sample, just_first_word = FALSE)
#'
proper <- function(x, just_first_word = TRUE) {

  if (just_first_word) {

    paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

  } else {

    gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl = TRUE)

  }

}



