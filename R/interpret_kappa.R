
#' @title
#' Interpretation of kappa statistic
#'
#' @description
#' Returns the interpretation of the strength of agreement determined by the
#' kappa statistic with a variety of different benchmark scales to choose from.
#' In practice, these can be used with other agreement statistics as well.
#'
#' @param kappa Numeric; kappa statistic
#' @param scale Which benchmark scale to use for interpretation. The default is
#'   that proposed by Landis and Koch (1977). Options include "Landis and Koch",
#'   "Fleiss", "Altman", "Cicchetti", and "Koo and Li".
#'
#' @references
#' \itemize{
#' \item \href{https://www.jstor.org/stable/2529310?seq=1#metadata_info_tab_contents}{Landis and Koch (1977)}
#' \item \href{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.456.3830&rep=rep1&type=pdf}{Fleiss (1981)}
#' \item \href{https://www.crcpress.com/Practical-Statistics-for-Medical-Research/Altman/p/book/9780412276309}{Altman (1991)}
#' \item \href{https://www.researchgate.net/publication/232556850_Guidelines_Criteria_and_Rules_of_Thumb_for_Evaluating_Normed_and_Standardized_Assessment_Instrument_in_Psychology}{Cicchetti (1994)}
#' \item \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/}{Koo and Li (2016)}
#'
#' }
#'
#' @import dplyr
#'
#' @return A string
#' @export
#'
#' @examples
#' interpret_kappa(0.5)
#' interpret_kappa(0.896)
#' interpret_kappa(-0.896)
#' interpret_kappa(0.15)
#' interpret_kappa(0.15, "Altman")
#'
interpret_kappa <- function(kappa,
                            scale = "Landis and Koch") {

  if (scale == "Landis and Koch") {

    dplyr::case_when(
      kappa < 0.00 ~ "poor",
      kappa < 0.20 ~ "slight",
      kappa < 0.40 ~ "fair",
      kappa < 0.60 ~ "moderate",
      kappa < 0.80 ~ "substantial",
      kappa < 1.00 ~ "almost perfect",
      kappa == 1.00 ~ "perfect"
    )

  } else if (scale == "Fleiss") {

    dplyr::case_when(
      kappa < 0.40 ~ "poor",
      kappa <= 0.75 ~ "intermediate to good",
      kappa <= 1.00 ~ "excellent"
    )

  } else if (scale == "Altman") {

    dplyr::case_when(
      kappa < 0.20 ~ "poor",
      kappa < 0.40 ~ "fair",
      kappa < 0.60 ~ "moderate",
      kappa < 0.80 ~ "good",
      kappa <= 1.00 ~ "very good"
    )

  } else if (scale == "Cicchetti") {

    dplyr::case_when(
      kappa < 0.40 ~ "poor",
      kappa < 0.60 ~ "fair",
      kappa < 0.75 ~ "good",
      kappa <= 1.00 ~ "excellent"
    )

  } else if (scale == "Koo and Li") {

    dplyr::case_when(
      kappa < 0.50 ~ "poor",
      kappa < 0.75 ~ "moderate",
      kappa < 0.90 ~ "good",
      kappa <= 1.00 ~ "excellent"
    )
  }

}



