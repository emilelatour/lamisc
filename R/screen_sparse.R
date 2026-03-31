#' Screen variables for sparse counts prior to regression
#'
#' Checks categorical variables for levels with low total counts or low event
#' counts. Supports Cox, logistic, Poisson, and linear regression. For
#' continuous variables, sparsity flags are always FALSE (no per-level cells).
#'
#' @param data A data frame.
#' @param vars Character vector of variable names to screen.
#' @param event Bare column name of the outcome/event variable. Set to `NULL`
#'   (default) for linear regression, where event counts are not applicable.
#' @param min_n Minimum acceptable total count per level. Default `5`.
#' @param min_events Minimum acceptable event count per level. Default `5`.
#'   Ignored when `event = NULL`.
#' @param regression One of `"cox"`, `"logistic"`, `"poisson"`, or `"linear"`.
#'   Controls how events are counted. `"cox"` and `"logistic"` count rows where
#'   the event is `1`, `TRUE`, or `"Yes"`. `"poisson"` sums the outcome per
#'   level. `"linear"` is equivalent to `event = NULL`. Default `"cox"`.
#'
#' @return A tibble with one row per variable-level combination and columns:
#'   `var_name`, `level`, `n_total`, `n_events` (NA for linear), `sparse_total`,
#'   `sparse_events` (NA for linear), `sparse_flag`.
#'
#' @examples
#' # Simulate a small dataset for examples
#' set.seed(42)
#' n <- 200
#' sim_df <- data.frame(
#'   # Categorical predictors
#'   treat     = sample(c("A", "B", "C"), n, replace = TRUE),
#'   stage     = factor(sample(c("I", "II", "III"), n, replace = TRUE,
#'                              prob = c(0.6, 0.3, 0.1))),  # sparse stage III
#'   sex       = sample(c("Male", "Female"), n, replace = TRUE),
#'   # Continuous predictor
#'   age       = round(rnorm(n, mean = 55, sd = 12)),
#'   # Outcomes
#'   event_bin = rbinom(n, 1, 0.3),          # binary (Cox / logistic)
#'   event_cnt = rpois(n, lambda = 2),        # count (Poisson)
#'   outcome_c = rnorm(n, mean = 10, sd = 3), # continuous (linear)
#'   # Survival time
#'   time      = rexp(n, rate = 0.1)
#' )
#'
#' vars <- c("treat", "stage", "sex", "age")
#'
#' # Cox
#' screen_sparse(sim_df, vars, event = event_bin, regression = "cox",
#'               min_n = 5, min_events = 5)
#'
#' # Logistic
#' screen_sparse(sim_df, vars, event = event_bin, regression = "logistic",
#'               min_n = 5, min_events = 5)
#'
#' # Poisson (sums outcome counts per level)
#' screen_sparse(sim_df, vars, event = event_cnt, regression = "poisson",
#'               min_n = 5, min_events = 5)
#'
#' # Linear (no event check)
#' screen_sparse(sim_df, vars, regression = "linear", min_n = 5)
#' screen_sparse(sim_df, vars)  # same — event = NULL by default
#' @export
screen_sparse <- function(data,
                          vars,
                          event      = NULL,
                          min_n      = 5,
                          min_events = 5,
                          regression = c("cox", "logistic", "poisson", "linear")) {

  regression <- match.arg(regression)

  # Treat linear or missing event as "no event check"
  check_events <- regression != "linear" && !missing(event) && !is.null(substitute(event))

  if (check_events) {
    event_sym  <- rlang::ensym(event)
    event_name <- rlang::as_name(event_sym)
  }

  purrr::map_dfr(vars, function(v) {

    x <- data[[v]]
    y <- if (check_events) data[[event_name]] else NULL

    # Drop rows missing the predictor or (if used) the event
    keep <- if (check_events) !is.na(x) & !is.na(y) else !is.na(x)
    x    <- x[keep]
    y    <- if (check_events) y[keep] else NULL

    if (is.character(x)) x <- factor(x)

    # ── Categorical ─────────────────────────────────────────────────────────
    if (is.factor(x)) {
      x <- droplevels(x)

      out <- tibble::tibble(var_name = v, level = levels(x)) |>
        dplyr::left_join(
          tibble::tibble(x = x) |>
            dplyr::count(x, name = "n_total") |>
            dplyr::rename(level = x),
          by = "level"
        ) |>
        dplyr::mutate(n_total = tidyr::replace_na(n_total, 0L))

      if (check_events) {
        event_counts <- if (regression == "poisson") {
          # Sum of the count outcome per level
          tibble::tibble(x = x, y = as.numeric(y)) |>
            dplyr::summarise(n_events = sum(y, na.rm = TRUE), .by = x) |>
            dplyr::rename(level = x)
        } else {
          # Cox / logistic: rows where event occurred
          tibble::tibble(x = x, y = y) |>
            dplyr::filter(y == 1 | y == TRUE | y == "Yes") |>
            dplyr::count(x, name = "n_events") |>
            dplyr::rename(level = x)
        }

        out <- out |>
          dplyr::left_join(event_counts, by = "level") |>
          dplyr::mutate(
            n_events      = tidyr::replace_na(n_events, 0L),
            sparse_events = n_events < min_events
          )
      } else {
        out <- dplyr::mutate(out, n_events = NA_integer_, sparse_events = NA)
      }

      out |>
        dplyr::mutate(
          sparse_total = n_total < min_n,
          sparse_flag  = if (check_events) sparse_total | sparse_events else sparse_total
        ) |>
        dplyr::select(var_name, level, n_total, n_events,
                      sparse_total, sparse_events, sparse_flag)

    # ── Continuous ───────────────────────────────────────────────────────────
    } else {
      tibble::tibble(
        var_name      = v,
        level         = "continuous",
        n_total       = length(x),
        n_events      = if (check_events) {
          if (regression == "poisson") sum(as.numeric(y), na.rm = TRUE)
          else                         sum(y == 1 | y == TRUE | y == "Yes", na.rm = TRUE)
        } else NA_integer_,
        sparse_total  = FALSE,
        sparse_events = if (check_events) FALSE else NA,
        sparse_flag   = FALSE
      )
    }
  })
}
