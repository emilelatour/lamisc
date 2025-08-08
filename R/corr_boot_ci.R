#' Bootstrap and Analytic Confidence Intervals for Correlation Coefficients
#'
#' Computes correlation coefficients (Pearson, Spearman, or Kendall) along with
#' bootstrap-based confidence intervals and, optionally, an analytic Fisher-\emph{z}
#' confidence interval for Pearson correlations.
#'
#' @param data A data frame containing the variables of interest.
#' @param x,y Column names (unquoted) specifying the variables to correlate.
#' @param method Correlation method; one of `"spearman"`, `"pearson"`, or `"kendall"`.
#' @param conf_level Confidence level for the intervals (default `0.95`).
#' @param R Number of bootstrap resamples (default `2000`).
#' @param ci_types Character vector of CI types to return. Valid values are:
#'   * `"bca"` – bias-corrected and accelerated bootstrap CI
#'   * `"basic"` – basic bootstrap CI
#'   * `"perc"` – percentile bootstrap CI
#'   * `"norm"` – normal bootstrap CI
#'   * `"fisherz"` – analytic Fisher-\emph{z} CI (Pearson only, no bootstrap required)
#'   One or more types can be requested; order determines output order.
#' @param seed Optional integer seed for reproducibility.
#' @param parallel Bootstrap parallelization method; passed to [boot::boot()]
#'   (`"no"`, `"multicore"`, or `"snow"`).
#' @param ncpus Number of cores for parallel execution (default from
#'   `getOption("mc.cores", 1)`).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{method}{Correlation method used.}
#'   \item{estimate}{Observed correlation coefficient.}
#'   \item{lower_ci, upper_ci}{Lower and upper confidence limits.}
#'   \item{ci_type}{Type of CI.}
#'   \item{conf_level}{Confidence level.}
#'   \item{R}{Number of bootstrap resamples (NA for analytic CIs).}
#'   \item{n}{Number of complete observations used.}
#'   \item{p_value}{Two-sided p-value from \code{\link[stats]{cor.test}}.}
#' }
#' The returned tibble has an attribute `"boot_object"` containing the `boot`
#' object from [boot::boot()] if bootstrap CIs were computed, or `NULL` otherwise.
#'
#' @details
#' For Pearson correlations, the analytic Fisher-\emph{z} interval is computed as:
#' \deqn{z = \tanh^{-1}(r), \quad SE = 1/\sqrt{n-3},}
#' \deqn{\mathrm{CI} = \tanh(z \pm z_{\alpha/2} \times SE).}
#'
#' Bootstrap CIs are computed via [boot::boot()] and [boot::boot.ci()]. If a
#' requested bootstrap CI type fails, a percentile CI is returned as a fallback
#' with a warning. If percentile also fails, NA bounds are returned.
#'
#' @importFrom boot boot boot.ci
#' @importFrom rlang ensym
#' @importFrom dplyr select filter
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom stats cor cor.test complete.cases pnorm qnorm quantile
#'
#' @examples
#' # Spearman with BCa (default)
#' corr_boot_ci(mtcars, mpg, wt, method = "spearman")
#'
#' # Pearson with multiple bootstrap CI types
#' corr_boot_ci(mtcars, mpg, wt, method = "pearson",
#'              ci_types = c("bca","basic","perc","norm"),
#'              R = 5000, seed = 42)
#'
#' # Kendall with percentile CI only
#' corr_boot_ci(mtcars, mpg, wt, method = "kendall",
#'              ci_types = "perc", R = 3000)
#'
#' # Pearson with Fisher-z analytic CI only (no bootstrap)
#' corr_boot_ci(mtcars, mpg, wt, method = "pearson", ci_types = "fisherz")
#'
#' # Pearson with both Fisher-z and BCa bootstrap CIs
#' corr_boot_ci(mtcars, mpg, wt, method = "pearson",
#'              ci_types = c("fisherz","bca"), R = 5000, seed = 42)
#'
#' @export


corr_boot_ci <- function(data, x, y,
                         method = c("spearman", "pearson", "kendall"),
                         conf_level = 0.95,
                         R = 2000,
                         ci_types = "bca",   # any of: "bca","basic","perc","norm","fisherz"
                         seed = NULL,
                         parallel = c("no","multicore","snow"),
                         ncpus = getOption("mc.cores", 1)) {
  method   <- match.arg(method)
  parallel <- match.arg(parallel)
  if (!is.null(seed)) set.seed(seed)

  # Tidy-eval & NA handling
  x_col <- ensym(x); y_col <- ensym(y)
  df <- data %>%
    select(x = !!x_col, y = !!y_col) %>%
    filter(complete.cases(.))
  n <- nrow(df)
  if (n < 3L) stop("Not enough complete cases after NA filtering.")

  # Validate ci_types (allow several)
  valid_ci <- c("bca","basic","perc","norm","fisherz")
  ci_types <- unique(tolower(ci_types))
  bad <- setdiff(ci_types, valid_ci)
  if (length(bad)) stop("Invalid ci_types: ", paste(bad, collapse = ", "),
                        ". Valid: ", paste(valid_ci, collapse = ", "))

  # Non-bootstrap p-value
  ct <- switch(
    method,
    spearman = cor.test(df$x, df$y, method = "spearman", exact = FALSE),
    pearson  = cor.test(df$x, df$y, method = "pearson"),
    kendall  = cor.test(df$x, df$y, method = "kendall")
  )
  p_value <- unname(ct$p.value)

  # Bootstrap statistic
  boot_fn <- function(dat, idx) {
    d <- dat[idx, , drop = FALSE]
    cor(d$x, d$y, method = method)
  }

  # Only run boot if needed
  need_boot <- any(ci_types %in% c("bca","basic","perc","norm"))
  boot_out <- if (need_boot) {
    boot::boot(data = df, statistic = boot_fn, R = R, parallel = parallel, ncpus = ncpus)
  } else {
    # Minimal object; we won't call boot.ci() on it
    list(t0 = cor(df$x, df$y, method = method), t = NA_real_)
  }

  # Helper: bootstrap CI with graceful fallback
  get_boot_ci <- function(type) {
    out <- tryCatch(boot::boot.ci(boot_out, type = type, conf = conf_level),
                    error = function(e) NULL)
    if (!is.null(out)) {
      mat <- switch(
        type,
        bca   = out$bca,
        perc  = out$percent,
        basic = out$basic,
        norm  = if (!is.null(out$normal))
                  cbind(NA, NA, NA, out$normal[2], out$normal[3]) else NULL,
        NULL
      )
      if (!is.null(mat)) {
        return(tibble(
          method = method,
          estimate = as.numeric(boot_out$t0),
          lower_ci = as.numeric(mat[,4]),
          upper_ci = as.numeric(mat[,5]),
          ci_type = type,
          conf_level = conf_level,
          R = R,
          n = n,
          p_value = p_value
        ))
      }
    }
    # Fallback to percentile if boot failed
    if (type != "perc") {
      q <- quantile(boot_out$t, probs = c((1 - conf_level)/2, 1 - (1 - conf_level)/2), na.rm = TRUE)
      warning(sprintf("CI type '%s' failed; returning percentile CI as fallback.", type))
      tibble(
        method = method,
        estimate = as.numeric(boot_out$t0),
        lower_ci = as.numeric(q[1]),
        upper_ci = as.numeric(q[2]),
        ci_type = paste0(type, "->perc_fallback"),
        conf_level = conf_level,
        R = R,
        n = n,
        p_value = p_value
      )
    } else {
      warning("Percentile CI failed; returning NA bounds.")
      tibble(
        method = method,
        estimate = as.numeric(boot_out$t0),
        lower_ci = NA_real_,
        upper_ci = NA_real_,
        ci_type = "perc_failed",
        conf_level = conf_level,
        R = R,
        n = n,
        p_value = p_value
      )
    }
  }

  # Helper: Fisher-z analytic CI (Pearson only)
  get_fisherz_ci <- function() {
    if (method != "pearson") {
      warning("Fisher-z CI is only defined for Pearson correlation; skipping.")
      return(NULL)
    }
    if (n <= 3L) {
      warning("Fisher-z CI requires n > 3; returning NA bounds.")
      return(tibble(
        method = method,
        estimate = as.numeric(cor(df$x, df$y, method = "pearson")),
        lower_ci = NA_real_,
        upper_ci = NA_real_,
        ci_type = "fisherz",
        conf_level = conf_level,
        R = NA_integer_,
        n = n,
        p_value = p_value
      ))
    }
    r_hat <- unname(cor(df$x, df$y, method = "pearson"))
    r_hat <- pmin(pmax(r_hat, -1 + 1e-15), 1 - 1e-15)  # guard against |r|=1
    z <- atanh(r_hat)
    se <- 1 / sqrt(n - 3)
    zcrit <- qnorm((1 + conf_level)/2)
    lo <- tanh(z - zcrit * se)
    hi <- tanh(z + zcrit * se)
    tibble(
      method = method,
      estimate = r_hat,
      lower_ci = as.numeric(lo),
      upper_ci = as.numeric(hi),
      ci_type = "fisherz",
      conf_level = conf_level,
      R = NA_integer_,
      n = n,
      p_value = p_value
    )
  }

  pieces <- map(ci_types, function(t) if (t == "fisherz") get_fisherz_ci() else get_boot_ci(t))
  res <- bind_rows(pieces[!vapply(pieces, is.null, logical(1))])

  attr(res, "boot_object") <- if (need_boot) boot_out else NULL
  res
}
