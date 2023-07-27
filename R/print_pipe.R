

#' @title
#' Print result of a pipe
#'
#' @description
#' Sometimes when I am working on a series of piped code, I want to see the
#' interim results when I am adding lines etc. It used to be that you could add
#' `%>% {.}` with the pipe command from `magrittr` package (you still can), but
#' this does not work with the native pipe `|>`. This is my workaround.
#'
#' @param x An object or value from a previous pipe to print.
#' @param n Number of rows to print.
#'
#' @details
#' A wrapper around `x %>% print()` or `x |> print()`
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(palmerpenguins)
#'
#'
#' penguins |>
#'   group_by(species) |>
#'   summarise(mean_bill_length_mm = mean(bill_length_mm, na.rm = TRUE)) |>
#'   print_pipe()
#'
#' penguins |>
#'   print_pipe()
#'
#' penguins |>
#'   print_pipe(n = Inf)

print_pipe <- function(x, n = NULL) {
  x |>
    print(n = n)
}



