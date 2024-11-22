#' Clean Invalid Characters in a Data Frame or Tibble
#'
#' This function cleans invalid characters in a data frame or tibble. It converts all character columns to UTF-8 encoding, replaces invalid characters with a specified substitution string, and removes occurrences of specific unwanted patterns (e.g., `<a0>`). The returned object retains the input type (data frame or tibble).
#'
#' @param data A data frame or tibble containing the data to be cleaned.
#' @param sub A string used to replace invalid characters. Default is a single space `" "`.
#' @param pattern A character string representing the pattern to remove. Default is `"<a0>"`.
#'
#' @return A cleaned data frame or tibble, depending on the input type. The structure of the input object is preserved.
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   col1 = c("valid", "invalid<a0>text"),
#'   col2 = c("another<a0>value", "valid"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example tibble
#' library(tibble)
#' tbl <- tibble(
#'   col1 = c("valid", "invalid<a0>text"),
#'   col2 = c("another<a0>value", "valid")
#' )
#'
#' # Clean the data frame
#' cleaned_df <- clean_invalid_characters(df)
#' cleaned_tbl <- clean_invalid_characters(tbl)
#'
#' @export
clean_invalid_characters <- function(data, sub = " ", pattern = "<a0>") {
  # Check input type
  is_tibble <- inherits(data, "tbl_df")

  # Convert all character columns to UTF-8 and replace invalid characters
  cleaned_data <- lapply(data, function(col) {
    if (is.character(col)) {
      col <- iconv(col, to = "UTF-8", sub = sub)
      col <- gsub(pattern, sub, col, fixed = TRUE)
    }
    col
  })

  # Reconstruct the data frame or tibble
  cleaned_data <- if (is_tibble) {
    tibble::as_tibble(cleaned_data)
  } else {
    as.data.frame(cleaned_data)
  }

  # Return the cleaned object
  return(cleaned_data)
}
