






#' @title Create a rank variable by group in a data frame
#'
#' @description
#' Rank within a group of specified variables in a data frame. I can't remember
#' where I saw this function originally but it's a simple clean way to do the
#' intended task.
#'
#' `rank_in_group` is intended to be used in a pipe where `dplyr::group_by()`
#' and `dplyr::arrange()` were applied already. See the example below for this
#' use.
#'
#' `rank_in_group2` takes a data frame, grouping variable, and arranging
#' variable as arguments and returns a data frame with a column, `rank` added.
#' Similar to `rank_in_group` but works on it's own outside a pipe.
#'
#' @param df A data frame
#' @param group_var A variable to group by
#' @param arrange_var A variable to arrange (descending) by
#'
#' @import dplyr
#' @import rlang
#'
#' @return A data frame with an added column `rank`
#' @export
#'
#' @examples
#' library(dplyr)
#' res <- iris %>%
#'   dplyr::group_by(Species) %>%
#'   dplyr::arrange(dplyr::desc(Sepal.Length)) %>%
#'   rank_in_group(.)
#'
#' # display first few results
#' res %>%
#'   dplyr::filter(rank <= 3)
#'
#' # There is also a way to do this just using dplyr:
#' by_species <- iris %>%
#'   dplyr::arrange(Species, Sepal.Length) %>%
#'   dplyr::group_by(Species) %>%
#'   dplyr::mutate(rank = rank(Sepal.Length, ties.method = "first"))
#'
#' by_species %>%
#'   dplyr::filter(rank <= 3)
#'

rank_in_group <- function(df) {

  df %>%
    mutate(constcol = 1) %>%
    mutate(rank = cumsum(constcol)) %>%
    dplyr::select(-constcol)

}

#' @rdname rank_in_group
#' @export
#' @examples
#' library(dplyr)
#'
#' res2 <- rank_in_group2(df = iris,
#'                        group_var = Species,
#'                        arrange_var = Sepal.Length)
#'
#' # display first few results
#' res2 %>%
#'   dplyr::filter(rank <= 3)

rank_in_group2 <- function(df, group_var, arrange_var) {

  group_var <- rlang::enquo(group_var)
  arrange_var <- rlang::enquo(arrange_var)

  df %>%
    dplyr::group_by(!! group_var) %>%
    dplyr::arrange(dplyr::desc(!! arrange_var)) %>%
    mutate(constcol = 1) %>%
    mutate(rank = cumsum(constcol)) %>%
    dplyr::select(-constcol)

}

