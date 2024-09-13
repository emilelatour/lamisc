

#' @title Check the counts before and after join
#'
#' @description
#' Before and after using joins, it's important to check the counts of the data
#' sets. This funtions give you the counts for the data you intend to join and
#' the resulting counts for all forms of joins (e.g. left, right, inner, semi,
#' anti, full).
#'
#' ## Inner join
#'
#' An `inner_join()` only keeps observations from `x` that have a matching key
#' in `y`.
#'
#' The most important property of an inner join is that unmatched rows in either
#' input are not included in the result. This means that generally inner joins
#' are not appropriate in most analyses, because it is too easy to lose
#' observations.
#'
#' ## Outer joins
#'
#' The three outer joins keep observations that appear in at least one of the
#' data frames:
#'
#' * A `left_join()` keeps all observations in `x`.
#'
#' * A `right_join()` keeps all observations in `y`.
#'
#' * A `full_join()` keeps all observations in `x` and `y`.
#'
#' ## Filtering joins
#'
#' * `semi_join()` return all rows from `x` with a match in `y`.
#' * `anti_join()` return all rows from `x` with**out** a match in `y`.
#'
#' @param x,y A pair of data frames, data frame extensions (e.g. a tibble), or
#'   lazy data frames (e.g. from dbplyr or dtplyr). See *Methods*, below, for
#'   more details.
#' @param by A join specification created with [join_by()], or a character
#'   vector of variables to join by.
#'
#'   If `NULL`, the default, `*_join()` will perform a natural join, using all
#'   variables in common across `x` and `y`. A message lists the variables so
#'   that you can check they're correct; suppress the message by supplying `by`
#'   explicitly.
#'
#'   To join on different variables between `x` and `y`, use a [join_by()]
#'   specification. For example, `join_by(a == b)` will match `x$a` to `y$b`.
#'
#'   To join by multiple variables, use a [join_by()] specification with
#'   multiple expressions. For example, `join_by(a == b, c == d)` will match
#'   `x$a` to `y$b` and `x$c` to `y$d`. If the column names are the same between
#'   `x` and `y`, you can shorten this by listing only the variable names, like
#'   `join_by(a, c)`.
#'
#'   [join_by()] can also be used to perform inequality, rolling, and overlap
#'   joins. See the documentation at [?join_by][join_by()] for details on
#'   these types of joins.
#'
#'   For simple equality joins, you can alternatively specify a character vector
#'   of variable names to join by. For example, `by = c("a", "b")` joins `x$a`
#'   to `y$a` and `x$b` to `y$b`. If variable names differ between `x` and `y`,
#'   use a named character vector like `by = c("x_a" = "y_a", "x_b" = "y_b")`.
#' @return
#' A tibble.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' check_joins(x = band_members,
#'             y = band_instruments,
#'             by = "name")
#'
#' check_joins(x = band_members,
#'             y = band_instruments,
#'             by = join_by(name))
#'
#'
#' band_members <- band_members |>
#'   mutate(name.x = name,
#'          name.a = name)
#'
#' band_instruments <- band_instruments |>
#'   mutate(name.y = name,
#'          name.b = name)
#'
#' check_joins(x = band_members,
#'             y = band_instruments,
#'             by = c("name.x" = "name.y"))
#'
#' check_joins(x = band_members,
#'             y = band_instruments,
#'             by = c("name.x" = "name.y",
#'                    "name.a" = "name.b"))

check_joins <- function(x, y, by = NULL) {

  # Ensure x and y are data frames
  if (!is.data.frame(x)) stop("Argument 'x' must be a data frame.")
  if (!is.data.frame(y)) stop("Argument 'y' must be a data frame.")


  # Determine join columns
  if (inherits(by, "dplyr_join_by")) {

    rev_by <- by

  } else if (is.vector(by) && !is.null(names(by))) {

    rev_by <- setNames(names(by), by)

  } else {

    rev_by <- by

  }


  # List of dimensions after different joins
  join_list <- list(
    dim(x),
    dim(y),
    dplyr::left_join(x, y, by = by) |> dim(),
    dplyr::right_join(x, y, by = by) |> dim(),
    dplyr::inner_join(x, y, by = by) |> dim(),
    dplyr::semi_join(x, y, by = by) |> dim(),
    dplyr::anti_join(x, y, by = by) |> dim(),
    dplyr::anti_join(y, x, by = rev_by) |> dim(),
    dplyr::full_join(x, y, by = by) |> dim()
  )


  tibble::tibble(
    join_type = c("Original X",
                  "Original Y",
                  "Left Join (X -> Y)",
                  "Right Join (Y -> X)",
                  "Inner Join",
                  "Semi Join (X in Y)",
                  "Anti Join (X - Y)",
                  "Anti Join (Y - X)",
                  "Full Join (X <-> Y)")) |>
    mutate(n_rows = purrr::map_int(.x = join_list,
                                   .f = ~ .x[1]),
           n_cols = purrr::map_int(.x = join_list,
                                   .f = ~ .x[2]))


}

