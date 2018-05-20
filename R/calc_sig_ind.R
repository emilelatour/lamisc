
#' @title
#' Returns the significance codes the R shows in regression output.
#'
#' @description
#' Signif. codes:  `0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1`
#'
#' @param p_value A p-value or vector of p-values. Can handle numeric or
#'   characters with text.
#' @param html Logical; if \code{TRUE}, escapes character suitable for markdown,
#'   else suitable for pdf or word.
#'
#' @importFrom dplyr case_when
#'
#' @return A character value or vector
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' pvals <- c(.133213, .06023, .004233, .000000134234, 1.0)
#' pvals_2 <- c(.133213, .06023, .004233, .000000134234, NA)
#' calc_sig_ind(pvals)
#' calc_sig_ind(pvals_2)
#'
#' df <- tibble::tibble(p = pvals,
#'                      p_2 = pvals_2)
#'
#' df %>%
#'   mutate(p_sig = calc_sig_ind(p),
#'          p_2_sig = calc_sig_ind(p_2)
#'   )
#'
calc_sig_ind <- function(p_value, html = TRUE) {

  p_value <- suppressWarnings(
    num_extract(p_value, as_char = FALSE)
  )

  if (html) {
    dplyr::case_when(
      p_value <= 0 ~ "",
      p_value <= 0.001 ~ "\\***",
      p_value <= 0.01 ~ "\\**",
      p_value <= 0.05 ~ "\\*",
      p_value <= 0.1 ~ ".",
      p_value <= 1 ~ "",
      TRUE ~ ""
    )
  } else {
    dplyr::case_when(
      p_value <= 0 ~ "",
      p_value <= 0.001 ~ "***",
      p_value <= 0.01 ~ "**",
      p_value <= 0.05 ~ "*",
      p_value <= 0.1 ~ ".",
      p_value <= 1 ~ "",
      TRUE ~ ""
    )
  }

}

