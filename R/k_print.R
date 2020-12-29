

#' @title
#' Kable print wrapper
#'
#' @description
#' A wrapper function for `knitr::kable` and `kableExtra` package.
#'
#' @param x An R object, typically a matrix or data frame.
#' @param format A character string. Possible values are latex, html, markdown,
#'   pandoc, and rst; this will be automatically determined if the function is
#'   called within knitr; it can also be set in the global option
#'   knitr.table.format. If format is a function, it must return a character
#'   string.
#' @param col_names A character vector of column names to be used in the table.
#' @param bootstrap_options A character vector for bootstrap table options.
#'   Please see package vignette or visit the w3schools' Bootstrap Page for more
#'   information. Possible options include basic, striped, bordered, hover,
#'   condensed and responsive.
#' @param escape Boolean; whether to escape special characters when producing
#'   HTML or LaTeX tables. When escape = FALSE, you have to make sure that
#'   special characters will not trigger syntax errors in LaTeX or HTML.
#' @param align Column alignment: a character vector consisting of 'l' (left),
#'   'c' (center) and/or 'r' (right). By default or if align = NULL, numeric
#'   columns are right-aligned, and other columns are left-aligned. If
#'   length(align) == 1L, the string will be expanded to a vector of individual
#'   letters, e.g. 'clc' becomes c('c', 'l', 'c'), unless the output format is
#'   LaTeX.
#' @param long_table Boolean; Whether to break a table across multiple pages or
#'   not.
#' @param latex_options A character vector for LaTeX table options. Please see
#'   package vignette for more information. Possible options include basic,
#'   striped, hold_position, HOLD_position, scale_down & repeat_header. striped
#'   will add alternative row colors to the table. It will imports LaTeX package
#'   xcolor if enabled. hold_position will "hold" the floating table to the
#'   exact position. It is useful when the LaTeX table is contained in a table
#'   environment after you specified captions in `kable()`. It will force the
#'   table to stay in the position where it was created in the document. A
#'   stronger version: HOLD_position requires the float package and specifies.
#'   scale_down is useful for super wide table. It will automatically
#'   adjust the table to page width. repeat_header in only meaningful in a
#'   longtable environment. It will let the header row repeat on every page in
#'   that long table.
#' @param caption The table caption.
#' @param cols A numeric value or vector indicating which column(s) to be selected.
#' @param widths A character string telling HTML & LaTeX how wide the column
#'   needs to be, e.g. "10cm", "3in" or "30em".
#'
#' @importFrom knitr opts_knit
#' @importFrom knitr is_latex_output
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra column_spec
#'
#' @return
#' A character vector of the table source code.
#'
#' @export
#'
#' @examples \dontrun{
#' k_print(x = head(iris),
#'         align = c("c", "c", "c", "c", "r"),
#'         format = "html",
#'         cols = c(2, 3, 4),
#'         widths = c("10em", "20em", "10em"))
#'         }

k_print <- function(x,
                    format,
                    col_names = NA,
                    bootstrap_options = c("striped"),
                    escape = TRUE,
                    align = NULL,
                    long_table = FALSE,
                    latex_options = "basic",
                    caption = NULL,
                    cols = NULL,
                    widths = NULL) {

  format = kable_format(format)

  k_tab <- knitr::kable(x,
                        format = format,
                        col.names = col_names,
                        booktabs = TRUE,
                        escape = escape,
                        align = align,
                        longtable = long_table,
                        caption = caption)

  k_tab <- kableExtra::kable_styling(kable_input = k_tab,
                                     full_width = FALSE,
                                     bootstrap_options = bootstrap_options,
                                     latex_options = latex_options)

  if (length(widths) > 0 & length(widths) != length(cols)) {
    stop("'cols' and 'widths' must be the same length.")
  }

  if (length(widths) > 0) {

    for (i in 1:length(cols)) {
      k_tab <- kableExtra::column_spec(kable_input = k_tab,
                                       column = cols[i],
                                       width = widths[i])
    }}

  return(k_tab)

}



#### Taken from knitr package --------------------------------

# https://github.com/yihui/knitr/blob/cf213e48eb47b84bb82950d134c564a1e537586d/R/table.R

# determine the table format
kable_format = function(format = NULL) {
  if (missing(format) || is.null(format)) format = getOption('knitr.table.format')
  if (is.null(format)) format = if (is.null(pandoc_to())) switch(
    out_format() %n% 'markdown',
    latex = 'latex', listings = 'latex', sweave = 'latex',
    html = 'html', markdown = 'pipe', rst = 'rst',
    stop('table format not implemented yet!')
  ) else if (isTRUE(knitr::opts_knit$get('kable.force.latex')) && knitr::is_latex_output()) {
    # force LaTeX table because Pandoc's longtable may not work well with floats
    # http://tex.stackexchange.com/q/276699/9128
    'latex'
  } else 'pipe'
  if (is.function(format)) format = format()
  # backward compatibility with knitr <= v1.28
  switch(format, pandoc = 'simple', markdown = 'pipe', format)
}


# https://github.com/yihui/knitr/blob/0e717be80cd29c99ffb65bd0b36de7269e9bd069/R/utils.R

# rmarkdown sets an option for the Pandoc output format from markdown
pandoc_to = function(x) {
  fmt = knitr::opts_knit$get('rmarkdown.pandoc.to')
  if (missing(x)) fmt else !is.null(fmt) && (fmt %in% x)
}

# if LHS is NULL, return the RHS
`%n%` = function(x, y) if (is.null(x)) y else x

# return the output format, or if current format is in specified formats
out_format = function(x) {
  fmt = knitr::opts_knit$get('out.format')
  if (missing(x)) fmt else !is.null(fmt) && (fmt %in% x)
}
