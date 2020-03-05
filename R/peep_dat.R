

#' @title
#' Peep at your data
#'
#' @description
#' Get a quick overview of your data set:
#'
#' + Number of observations,
#' + Number of variables,
#' + Number of duplicates,
#' + Number of rows with missing values,
#' + Number of columns with missing values,
#' + Number of cells with missing values.
#'
#' This is meant to just give some quick counts. I like to "peep" my data set
#' and paste this output in my code as I am cleaning and munging. For a deeper
#' look, I suggest `dplyr::glimpse()` which is where the inspiration for this
#' function except I don't always want to see the columns and I want more
#' details than `base::dim()`.
#'
#' `janitor::get_dupes` is great for exploring duplicates. The `naniar` package
#' is best for exploring the missing values in your data.
#'
#' @param data A data frame or tbl_df.
#'
#' @importFrom glue glue
#' @importFrom scales number
#' @importFrom scales percent
#' @importFrom stats complete.cases
#'
#' @export
#'
#' @examples
#' peep_dat(mtcars)
#'
#' peep_dat(datasets::airquality)
peep_dat <- function(data) {

  #### Calculate the values --------------------------------

  dims <- base::dim(data)

  n_rows <- dims[[1]]
  n_cols <- dims[[2]]

  n_dups <- sum(base::duplicated(data))

  n_missing_rows <- sum(!stats::complete.cases(data))
  p_missing_rows <- n_missing_rows / n_rows

  n_missing_cols <- sum(sapply(data, function(x) any(is.na(x))))
  p_missing_cols <- n_missing_cols / n_cols

  n_missing_vals <- table(sapply(is.na(data), function(x) {factor(x, levels = c("TRUE", "FALSE"))}))["TRUE"]
  p_missing_vals <- n_missing_vals / (n_rows * n_cols)


  #### Format for printing --------------------------------

  dims <- scales::number(dims,
                         accuracy = 1,
                         big.mark = ",")


  n_missing_rows <- scales::number(n_missing_rows,
                                   accuracy = 1,
                                   big.mark = ",")

  p_missing_rows <- scales::percent(p_missing_rows,
                                   accuracy = 0.1,
                                   big.mark = ",")


  n_missing_cols <- scales::number(n_missing_cols,
                                   accuracy = 1,
                                   big.mark = ",")

  p_missing_cols <- scales::percent(p_missing_cols,
                                   accuracy = 0.1,
                                   big.mark = ",")


  n_missing_vals <- scales::number(n_missing_vals,
                                   accuracy = 1,
                                   big.mark = ",")

  p_missing_vals <- scales::percent(p_missing_vals,
                                   accuracy = 0.1,
                                   big.mark = ",")


  #### Glue it together --------------------------------

  obs_text <- glue::glue("Observations: {dims[[1]]}")
  vars_text <- glue::glue("Variables: {dims[[2]]}")
  dups_text <- glue::glue("Duplicates: {n_dups}")
  rows_miss_text <- glue::glue("Rows with any missing: {n_missing_rows} ({p_missing_rows})")
  cols_miss_text <- glue::glue("Columns with any missing: {n_missing_cols} ({p_missing_cols})")
  vals_miss_text <- glue::glue("Missing values: {n_missing_vals} ({p_missing_vals})")


  #### Return the text --------------------------------

  cat(obs_text, "\n")
  cat(vars_text, "\n")
  cat(dups_text, "\n")
  cat(rows_miss_text, "\n")
  cat(cols_miss_text, "\n")
  cat(vals_miss_text, "\n")

}




