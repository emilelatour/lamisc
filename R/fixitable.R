
library(dplyr)
library(rlang)
library(tibble)


#### fixitable --------------------------------

#' @title
#' Fix-it table.
#'
#' @description
#' Look-up table (aka dictionary table) to replace values in a data frame with
#' those given in another data frame
#'
#' Paraphrasing @ryantimpe -- If you need to make one change, then use an ifelse
#' (preferably `dplyr::if_else`). If you need to change a few values, then use
#' `dplyr::case_when`. Everything else is a join.
#'
#' Dictionary tables are a super useful concept when you need to make many
#' changes to variables. This function allows you to use one data frame with a
#' column of variable names, old values, and new values to recode and revalue
#' columns in the data frame you are working with. One variable at a time, you
#' can do this with a join. Here you can perform the task for many variables at
#' one time after you have the lookup table set up.
#'
#' To set up the dictionary table, it is recommended to use Excel or something
#' to hand edit a table with a column `var` for variable name, `old_value` for
#' the unique values currently in your data frame, and `new_value` for the
#' values that you want to change the old ones to. Then use the outstanding
#' package/addin by Miles McBain `datapasta` to paste as a tribble into your
#' script.
#'
#' Big thanks to Hilary Parker for this inspiration and idea.
#'
#' @references
#'
#' \url{https://twitter.com/hspter/status/948646331677118465}
#'
#' \url{https://twitter.com/ryantimpe/status/948671446418690050}
#'
#' \url{https://github.com/MilesMcBain/datapasta}
#'
#' \url{http://stat545.com/bit008_lookup.html}
#'
#' \url{https://www.tjmahr.com/recode-values-with-character-subsetting/}
#'
#' \url{https://stackoverflow.com/questions/42613167/replace-values-in-dataframe-column-based-on-match-in-second-data-frame-columns}
#'
#' @import dplyr
#' @import rlang
#' @importFrom rlang .data
#'
#' @param x Vector to modify
#' @param lookup A data frame or tibble. Must contain columns `var`,
#'   `old_value`, and `new_value`.
#'
#' @return A modified version of `x` that replaces the `old_values` with the
#'   `new_values`. Class is "character"; unfortunatley the original class is not
#'   preserved, but this is a minor sacrifice.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' #### Example 1 --------------------------------
#'
#' df <- tibble::tribble(
#'   ~foo, ~bar, ~baz,
#'   3L,  "a",  "A",
#'   1L,  "b",  "B",
#'   2L,  "c",  "C",
#'   2L,  "c",  "A",
#'   2L,  "c",  "A",
#'   3L,  "c",  "B",
#'   3L,  "a",  "B",
#'   1L,  "a",  "B",
#'   1L,  "d",  "C"
#' )
#'
#' # Created in excel and pasted as tribble using the datapasta addin.
#' # NOTE: lookup must contain the columns named var, old_value, and new_value
#' lookup <- tibble::tribble(
#'   ~var, ~old_value, ~new_value,
#'   "foo",        "1",        "a",
#'   "foo",        "2",        "b",
#'   "foo",        "3",        "c",
#'   "bar",        "a",        "A",
#'   "bar",        "b",        "B",
#'   "bar",        "c",        "C",
#'   "baz",        "A",        "X",
#'   "baz",        "B",        "Y",
#'   "baz",        "C",        "Z"
#' )
#'
#' df %>%
#'   mutate(new = fixitable(x = foo, lookup = lookup))
#'
#' df %>%
#'   mutate(foo = fixitable(x = foo, lookup = lookup))
#'
#' df %>%
#'   mutate(bar = fixitable(x = bar, lookup = lookup))
#'
#' df %>%
#'   mutate_at(.vars = vars(foo:baz),
#'             .funs = funs(fixitable(., lookup = lookup)))
#'
#' df %>%
#'   mutate(bar = factor(bar)) %>%
#'   mutate(bar = fixitable(x = bar, lookup = lookup))
#'
#'
#'
#' #### Example 2 --------------------------------
#'
#' mini_gap <- tibble::tribble(
#'   ~country, ~continent, ~year, ~lifeExp,
#'   "Belgium",   "Europe", 2002L,    78.32,
#'   "Belgium",   "Europe", 2007L,   79.441,
#'   "Canada", "Americas", 2002L,    79.77,
#'   "Canada", "Americas", 2007L,   80.653,
#'   "Mexico", "Americas", 2002L,   74.902,
#'   "Mexico", "Americas", 2007L,   76.195,
#'   "United States", "Americas", 2002L,    77.31,
#'   "United States", "Americas", 2007L,   78.242
#' )
#'
#'
#' lookup <- tibble::tribble(
#'   ~var,      ~old_value, ~new_value,
#'   "country",       "Belgium",   "waffle",
#'   "country",        "Canada",  "poutine",
#'   "country", "United States",  "Twinkie",
#'   "year",          "2002",       "02",
#'   "year",          "2007",       "07",
#'   "continent",        "Europe",      "Eur",
#'   "continent",      "Americas",     "Amer"
#' )
#'
#' mini_gap %>%
#'   mutate(food = fixitable(x = country, lookup = lookup))
#'
#' mini_gap %>%
#'   mutate(year = fixitable(x = year, lookup = lookup))
#'
#' mini_gap %>%
#'   mutate(continent = fixitable(x = continent, lookup = lookup))
#'
#' mini_gap  %>%
#'   mutate_at(.vars = vars(country, continent, year),
#'             .funs = funs(fixitable(x = ., lookup = lookup)))
#'

fixitable <- function(x, lookup) {

  x_enq <- rlang::enquo(x)
  x_name <- rlang::quo_name(x_enq)

  y <- lookup %>%
    dplyr::filter(.data$var == x_name)

  y$new_value[base::match(x, y$old_value)]

}


