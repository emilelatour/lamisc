


#' @title
#' flextable wrapper
#'
#' @description
#' A wrapper function for `flextable::flextable()` with some of my preferred
#' formatting for simple tables.
#'
#'
#' @param x dataset
#' @param font_name single character value, the font family name. With Word and
#'   PowerPoint output, the value specifies the font to be used to format
#'   characters in the Unicode range (U+0000-U+007F). For "all" table parts.
#' @param font_size integer value (points). For "all" table parts.
#' @param align Column alignment: a character vector consisting of 'l' (left),
#'   'c' (center), 'r' (right), and/or 'j' (justify). By default or if align =
#'   NULL, numeric columns are right-aligned, and other columns are
#'   left-aligned. If length(align) == 1L, the string will be expanded to a
#'   vector of individual letters, e.g. 'clc' becomes c('c', 'l', 'c').
#' @param align_j Column selection for `align` argument. Default is
#'   `1:length(align)`.
#' @param auto_fit Logical (Default is FALSE). See `flextable::autofit()`
#' @param width width in inches as a character vector.
#' @param width_j columns selection for width.
#' @param width_unit unit for width, one of "in", "cm", "mm".
#' @param col_nms Names of columns to be labelled.
#' @param col_lbls Labels to apply to columns different from column names.
#' @param title The text for the title.
#' @param subtitle The text for the subtitle which will be displayed below the
#'   title.
#' @param footer The text for the footer which will be displayed below the body
#'   of the table.
#' @param title_size Font size for the title. Default is 16.
#' @param subtitle_size  Font size for the subtitle. Default is 11.
#' @param footer_size  Font size for the footer Default is 11.
#' @param na_dflt,nan_dflt string to be used for NA and NaN values.
#'
#' @importFrom flextable add_footer_lines
#' @importFrom flextable add_header_lines
#' @importFrom flextable align
#' @importFrom flextable align
#' @importFrom flextable autofit
#' @importFrom flextable bold
#' @importFrom flextable flextable
#' @importFrom flextable font
#' @importFrom flextable fontsize
#' @importFrom flextable fp_border_default
#' @importFrom flextable hline
#' @importFrom flextable hline_top
#' @importFrom flextable set_flextable_defaults
#' @importFrom flextable set_header_labels
#' @importFrom flextable width
#' @importFrom officer fp_border
#' @importFrom rlang list2
#' @importFrom stringr str_squish
#'
#' @return
#' A flextable
#'
#' @export
#'
#' @examples
#' flex_print(x = head(iris))
#'
#' flex_print(x = head(iris),
#'            font_name = "Lato",
#'            title = "Edgar Anderson's Iris Data",
#'            title_size = 16,
#'            subtitle = "Just the head")
#'
#' flex_print(x = head(iris),
#'            font_name = "Lato",
#'            title = "Edgar Anderson's Iris Data",
#'            title_size = 16)
#'
#' flex_print(x = head(iris),
#'            font_name = "Times New Roman",
#'            title = "Edgar Anderson's Iris Data",
#'            title_size = 16,
#'            footer = "The data were collected by Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin of the American Iris Society, 59, 2–5.")
#'
#' flex_print(x = head(iris),
#'            align = c("c"),
#'            col_lbls = c("Sepal Length",
#'                         "Sepal Width",
#'                         "Petal Length",
#'                         "Petal Width",
#'                         "Species"))
#'
#'
#' flex_print(x = head(iris),
#'            align = c("c"),
#'            col_nms = names(head(iris)),
#'            col_lbls = c("Sepal\nLength",
#'                         "Sepal\nWidth",
#'                         "Petal Length",
#'                         "Petal Width",
#'                         "Species"),
#'            font_size = 16)

flex_print <- function(x,
                       font_name = "Arial",
                       font_size = 11,
                       align = NULL,
                       align_j = NULL,
                       auto_fit = TRUE,
                       width = NULL,
                       width_j = NULL,
                       width_unit = "in",
                       col_nms = NULL,
                       col_lbls = NULL,
                       title = NULL,
                       subtitle = NULL,
                       footer = NULL,
                       title_size = 16,
                       subtitle_size = 11,
                       footer_size = 11,
                       na_dflt = "NA",
                       nan_dflt = "NaN") {


  # Set the defaults for NA and NaN
  na_def <- flextable::set_flextable_defaults(na_str = na_dflt, nan_str = nan_dflt)

  # Get the dimensions of x
  x_dim <- dim(x)

  # Get column names for later maybe
  if (is.null(col_nms)) {
    col_nms <- names(x)
  }


  # Make a flextable object
  x <- x |>
    flextable::flextable()

  # Autofit if TRUE
  if (auto_fit) {
    x <- x |>
      flextable::autofit()
  }


  # Set columns width
  if (!is.null(width)) {
    x <- x |>
      flextable::width(j = width_j,
                       width = width,
                       unit = width_unit)
  }


  # Set text alignment
  if (!is.null(align)) {

    align <- unlist(strsplit(align, ""))

    n_cols <- x_dim[[2]]

    # Pad the end with the last element in align
    align_pad <- n_cols - length(align)
    align_pad <- max(0, align_pad)

    align <- c(align,
               rep(align[length(align)], align_pad))

    align <- align[1:n_cols]

    if (is.null(align_j)) {
      align_j <- c(1:length(align))
    }

    x <- x |>
      flextable::align(j = align_j,
                       align = align,
                       part = "all")
  }


  # Column labels
  if (!is.null(col_lbls)) {

    header_list <- rlang::list2(!!! setNames(col_lbls, col_nms))

    x <- x |>
      flextable::set_header_labels(values = header_list)

  }


  # Apply font size
  x <- x |>
    flextable::fontsize(size = font_size,
                        part = "all")

  # Apply font name
  x <- x |>
    flextable::font(fontname = font_name,
                    part = "all")


  # Bold column labels
  x <- x |>
    flextable::bold(i = 1, bold = TRUE, part = "header")


  # Subtitle and title
  if (!is.null(subtitle) & !is.null(title)) {

    title <- stringr::str_squish(title)
    subtitle <- stringr::str_squish(subtitle)

    x <- x |>
      flextable::add_header_lines(values = "") |>  # add Padding
      flextable::add_header_lines(values = subtitle) |>  # add subtitle
      flextable::add_header_lines(values = title) |>  # add title
      flextable::hline_top(border = flextable::fp_border_default(width = 0), part = "header") |>
      flextable::border_inner_h(border = flextable::fp_border_default(width = 0), part = "header") |>
      flextable::align(i = c(1, 2), j = 1, align = "left", part = "header") |>
      flextable::fontsize(i = 1, size = title_size, part = "header") |>
      flextable::fontsize(i = 2, size = subtitle_size, part = "header") |>
      flextable::bold(i = 2, bold = FALSE, part = "header")


    x <- x |>
      flextable::hline_top(part = "header",
                           border = officer::fp_border(color = "black",
                                                       width = 2.0,
                                                       style = "solid")) |>
      flextable::hline(i = 3,
                       part = "header",
                       border = officer::fp_border(color = "#666666",
                                                   width = 1.5,
                                                   style = "solid"))


  }

  # title only
  if (is.null(subtitle) & !is.null(title)) {

    title <- stringr::str_squish(title)

    x <- x |>
      flextable::add_header_lines(values = "") |>  # add Padding
      flextable::add_header_lines(values = title) |>  # add title
      flextable::hline_top(border = flextable::fp_border_default(width = 0), part = "header") |>
      flextable::border_inner_h(border = flextable::fp_border_default(width = 0), part = "header") |>
      flextable::align(i = 1, j = 1, align = "left", part = "header") |>
      flextable::fontsize(i = 1, size = title_size, part = "header")


    x <- x |>
      flextable::hline_top(part = "header",
                           border = officer::fp_border(color = "black",
                                                       width = 2.0,
                                                       style = "solid")) |>
      flextable::hline(i = 2,
                       part = "header",
                       border = officer::fp_border(color = "#666666",
                                                   width = 1.5,
                                                   style = "solid"))

  }

  if (!is.null(footer)) {

    title <- stringr::str_squish(footer)

    x <- x |>
      flextable::add_footer_lines(footer) |>
      # flextable::hline_bottom(part = "footer",
      #                         border = officer::fp_border(color = "#666666",
      #                                                     width = 0.25,
      #                                                     style = "solid")) |>
      flextable::font(fontname = font_name,
                      part = "footer") |>
      flextable::fontsize(i = 1, size = footer_size, part = "footer")


  }


  # Set the defaults for NA and NaN back to normal
  do.call(flextable::set_flextable_defaults, na_def)


  return(x)

}
