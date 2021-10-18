
#' @title 
#' Wrapper to get a glimpse of your data
#' 
#' @description 
#' Wrapper around `dplyr::glimpse()` to save me typing something over and over
#' again.
#' 
#' glimpse() is like a transposed version of print(): columns run down the page,
#' and data runs across. This makes it possible to see every column in a data
#' frame. It's a little like str() applied to a data frame but it tries to show
#' you as much data as possible. (And it always shows the underlying data, even
#' when applied to a remote data source.)
#'
#' @param x An object to glimpse at.
#' @param width Width of output: defaults to the setting of the width option (if
#'   finite) or the width of the console.
#' @param ... Unused, for extensibility.
#' 
#' @importFrom dplyr glimpse
#'
#' @return
#' x original x is (invisibly) returned, allowing glimpse() to be used within a
#' data pipe line.
#' 
#' @export
#'
#' @examples
#' gl(mtcars)
#' 
#' gl(nycflights13::flights)
gl <- function(x, width = NULL, ...) { 
  
  dplyr::glimpse(x, width = NULL, ...)
}



