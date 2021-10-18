
#' @title 
#' Pad a string with the maximum width
#' 
#' @description 
#' Vectorised over string, width and pad. Pad to the maximum width in a string.
#'
#' @param string A character vector.
#' @param width Minimum width of padded strings.
#' @param side Side on which padding character is added (left, right or both).
#' @param pad Single padding character (default is a space).
#' 
#' @importFrom stringr str_pad
#'
#' @return
#' A character vector.
#' 
#' @export
#'
#' @examples
#' x <- c("abc", "123456", "defg")
#' 
#' max_pad(x) 
#' max_pad(x, side = "right") 
#' max_pad(x, pad = "0") 
#' max_pad(x, side = "right", pad = "0") 
#' max_pad(x, side = "both", pad = "0") 
max_pad <- function(string, width = 0, side = c("left", "right", "both"), pad = " ") {
  # use widest element if bigger than `width`
  width <- max(c(nchar(string), width))
  # sprintf(paste0(prefix, "%0", width, "d"), x)
  stringr::str_pad(string, width = width, side = side, pad = pad)
}

# zero_pad <- function(x, prefix = "", width = 0) {
#   # use widest element if bigger than `width`
#   width <- max(c(nchar(x), width))
#   sprintf(paste0(prefix, "%0", width, "d"), x)
# }

