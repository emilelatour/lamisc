#' @title
#' gt wrapper
#'
#' @description
#' A wrapper function for `gt::gt()` with some of my preferred
#' formatting for simple tables.
#'
#'
#' @param x A data.frame object or a tibble (tbl_df).
#' @param font_name single character value, the font family name. With Word and
#'   PowerPoint output, the value specifies the font to be used to format
#'   characters in the Unicode range (U+0000-U+007F). For "all" table parts.
#' @param font_size For table body. Can be specified as a single-length
#'   character vector with units of pixels (e.g., 12px) or as a percentage
#'   (e.g., 80\%). If provided as a single-length numeric vector, it is assumed
#'   that the value is given in units of pixels. The px() and pct() helper
#'   functions can also be used to pass in numeric values and obtain values as
#'   pixel or percentage units.
#' @param align Column alignment: a character vector consisting of 'l' (left),
#'   'c' (center), 'r' (right), and/or 'j' (justify). By default or if align =
#'   NULL, numeric columns are right-aligned, and other columns are
#'   left-aligned. If length(align) == 1L, the string will be expanded to a
#'   vector of individual letters, e.g. 'clc' becomes c('c', 'l', 'c').
#' @param align_j Column selection for `align` argument. Default is
#'   `1:length(align)`.
#' @param width A numeric vector with desired column width in absolute pixel values.
#' @param width_j A numeric vector with column indicesto select for width.
#' @param col_nms Names of columns to be labelled.
#' @param col_lbls Labels to apply to columns different from column names.
#' @param title The text for the title.
#' @param subtitle The text for the subtitle which will be displayed below the
#'   title.
#' @param footer The text for the footer which will be displayed below the body
#'   of the table.
#' @param row_stripes Logical indicating whether to include row stripes.
#' @param row_lines Logical indicating whether to include row lines.
#' @param title_size Font size for the title. Can be specified as a
#'   single-length character vector with units of pixels (e.g., 12px) or as a
#'   percentage (e.g., 80\%). If provided as a single-length numeric vector, it
#'   is assumed that the value is given in units of pixels. The px() and pct()
#'   helper functions can also be used to pass in numeric values and obtain
#'   values as pixel or percentage units.
#' @param subtitle_size  Font size for the subtitle. Can be specified as a
#'   single-length character vector with units of pixels (e.g., 12px) or as a
#'   percentage (e.g., 80\%). If provided as a single-length numeric vector, it
#'   is assumed that the value is given in units of pixels. The px() and pct()
#'   helper functions can also be used to pass in numeric values and obtain
#'   values as pixel or percentage units.
#' @param footer_size  Font size for the footer. Can be specified as a
#'   single-length character vector with units of pixels (e.g., 12px) or as a
#'   percentage (e.g., 80\%). If provided as a single-length numeric vector, it
#'   is assumed that the value is given in units of pixels. The px() and pct()
#'   helper functions can also be used to pass in numeric values and obtain
#'   values as pixel or percentage units.
#' @param span_lbls a named list where the name is the label that gets passed to
#'   `gt::tab_spanner()` and the slot is a character vector of column names.
#'
#' @importFrom gt gt
#' @importFrom gt cell_text
#' @importFrom gt cols_width
#' @importFrom gt cols_align
#' @importFrom gt cols_label
#' @importFrom gt md
#' @importFrom gt tab_options
#' @importFrom gt tab_header
#' @importFrom gt tab_footnote
#' @importFrom gt opt_row_striping
#' @importFrom gt tab_style
#' @importFrom gt cells_column_labels
#' @importFrom gt cells_column_spanners
#' @importFrom gt px
#'
#' @importFrom glue glue
#' @importFrom dplyr case_match
#' @importFrom dplyr everything
#' @importFrom rlang list2
#' @importFrom rlang !!!
#'
#' @return
#' A customized gt object.
#'
#' @export
#'
#' @examples
#' gt_print(x = head(iris))
#'
#' gt_print(x = head(iris),
#'          font_name = "Lato",
#'          title = "Edgar Anderson's Iris Data",
#'          title_size = 16,
#'          subtitle = "Just the head")
#'
#' gt_print(x = head(iris),
#'          font_name = "Lato",
#'          title = "Edgar Anderson's Iris Data",
#'          title_size = 16)
#'
#' gt_print(x = head(iris),
#'          font_name = "Times New Roman",
#'          title = "Edgar Anderson's Iris Data",
#'          title_size = 16,
#'          footer = "The data were collected by Anderson, Edgar (1935). The irises of the Gaspe Peninsula, Bulletin of the American Iris Society, 59, 2–5.")
#'
#' gt_print(x = head(iris),
#'          align = c("c"),
#'          col_lbls = c("Sepal Length",
#'                       "Sepal Width",
#'                       "Petal Length",
#'                       "Petal Width",
#'                       "Species"))
#'
#'
#' gt_print(x = head(iris),
#'          align = c("c"),
#'          col_nms = names(head(iris)),
#'          col_lbls = c("Sepal   Length",
#'                       "Sepal\nWidth",
#'                       "Petal Length",
#'                       "Petal Width",
#'                       "Species"),
#'          font_size = 16)
#'
#'
#' gt_print(x = head(iris),
#'          align = c("c"),
#'          col_lbls = c("Sepal   Length",
#'                       "Sepal<br />Width",
#'                       "Petal Length",
#'                       "Petal Width",
#'                       "Species"))
#'
#'
#' gt_print(x = head(iris),
#'          align = c("c"),
#'          col_lbls = c("Length",
#'                       "Width",
#'                       "Length",
#'                       "Width",
#'                       "Species"),
#'          span_lbls = list("Sepal" = c("Sepal.Length",
#'                                       "Sepal.Width"),
#'                           "Petal" = c("Petal.Length",
#'                                       "Petal.Width")))

gt_print <- function(x,
                     font_name = "Arial",
                     font_size = NULL,
                     title = NULL,
                     subtitle = NULL,
                     footer = NULL,
                     row_stripes = FALSE,
                     row_lines = FALSE,
                     align = NULL,
                     align_j = NULL,
                     col_nms = NULL,
                     col_lbls = NULL,
                     width = NULL,
                     width_j = NULL,
                     title_size = NULL,
                     subtitle_size = NULL,
                     footer_size = NULL,
                     span_lbls = NULL) {



  # Get the dimensions of x
  x_dim <- dim(x)

  # Get column names for later
  col_nms_0 <- names(x)

  if (is.null(col_nms)) {
    col_nms <- names(x)
  }


  # Make a gt object
  x <- x |>
    gt::gt()

  # Set columns width
  if (!is.null(width) & is.null(width_j)) {

    x <- x |>
      gt::cols_width(as.formula(glue::glue("dplyr::everything() ~ gt::px({width})")))

  } else if (!is.null(width)) {

    # Pad the end with the last element in align
    w_pad <- length(width_j) - length(width)
    w_pad <- max(0, w_pad)

    width <- c(width,
               rep(width[length(width)], w_pad))

    w_nms <- col_nms_0[width_j]

    for (i in 1:length(width_j)) {

      x <- x |>
        gt::cols_width(as.formula(glue::glue("{w_nms[i]} ~ gt::px({width[i]})")))

    }

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

    # "left", "center", "right", "justify"
    align <- dplyr::case_match(align,
                               "l" ~ "left",
                               "c" ~ "center",
                               "r" ~ "right",
                               "j" ~ "justify")


    for (i in 1:n_cols) {

      x <- gt::cols_align(data = x,
                          align = align[i],
                          columns = col_nms_0[i])

    }

  }


  # Column labels
  if (!is.null(col_lbls)) {

    # col_lbls <- gt::md(col_lbls)
    #
    # header_list <- rlang::list2(!!! setNames(col_lbls, col_nms))
    #
    # x <- x |>
    #   gt::cols_label(.list = header_list)

    col_lbls <- stringr::str_replace(string = col_lbls,
                                     pattern = "\\n",
                                     replacement = "<br />")


    for (i in 1:length(col_lbls)) {

      x <- x |>
        gt::cols_label(as.formula(glue::glue("{col_nms[i]} ~ gt::md('{col_lbls[i]}')")))

    }

  }

  # Bold column labels
  x <- x |> tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(columns = col_nms_0)
    )


  # Add tab spanner labels
  if (!is.null(span_lbls)) {

    for (i in 1:length(span_lbls)) {

      span_nm <- stringr::str_replace(string = names(span_lbls[i]),
                                      pattern = "\\n",
                                      replacement = "<br />")

      x <- x |>
        gt::tab_spanner(label = gt::md(span_nm),
                        columns = span_lbls[[i]])

    }

    x <- x |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_spanners(spanners = dplyr::everything())
      )

  }





  # Apply font size
  x <- x |>
    gt::tab_options(table.font.size = font_size,
                    heading.title.font.size = title_size,
                    heading.subtitle.font.size = subtitle_size,
                    footnotes.font.size = footer_size)

  # Apply font name
  x <- x |>
    gt::tab_options(table.font.names = font_name)

  # Subtitle and title
  if (!is.null(title) & !is.null(subtitle)) {
    x <- x |>
      gt::tab_header(
        title = gt::md(glue::glue("**{title}**")),
        subtitle = gt::md(subtitle)
      )
  } else if (!is.null(title)) {
    x <- x |>
      gt::tab_header(
        title = gt::md(title)
      )
  } else if (!is.null(subtitle)) {
    x <- x |>
      gt::tab_header(
        title = "",
        subtitle = gt::md(subtitle)
      )
  }


  if (!is.null(footer)) {

    x <- x |>
      gt::tab_footnote(footnote = footer)

  }

  # Table options
  x <- x |>
    gt::tab_options(heading.title.font.size = NULL,
                    heading.title.font.weight = "bolder",
                    heading.subtitle.font.size = NULL,
                    heading.subtitle.font.weight = NULL,
                    heading.align = "left")


  # Row lines
  if (row_lines) {

    x <- x |>
      gt::tab_options(table_body.hlines.style = NULL)

  } else {

    x <- x |>
      gt::tab_options(table_body.hlines.style = "all")

  }

  # Zebra stripes
  if (row_stripes) {

    x <- x |>
      gt::opt_row_striping(row_striping = TRUE)

  } else {

    x <- x |>
      gt::opt_row_striping(row_striping = FALSE) |>
      # Fix to keep stripes from showing up in quarto
      # https://github.com/quarto-dev/quarto-cli/issues/6945
      gt::tab_options(quarto.disable_processing = TRUE)

  }


  return(x)

}




