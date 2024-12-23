#' Pipe operator
#'
#' Like \code{dplyr}, \code{lamisc} also uses the pipe function, \code{\%>\%}
#' to turn function composition into a series of imperative statements.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


# From forcats forcats:::check_factor
check_factor <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is.character(x)) {
    factor(x)
  } else if (is.factor(x)) {
    x
  } else {
    cli::cli_abort(
      "{.arg {arg}} must be a factor or character vector, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}


