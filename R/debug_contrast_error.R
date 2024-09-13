#' @name debug_contrast_error
#' @aliases debug_contrast_error2
#'
#' @title
#' Debug "contrasts can be applied only to factors with 2 or more levels" error?
#'
#' @description
#' When fitting models using `lm` and `glm`, sometimes you can get a mystery
#' error that says that "contrasts can be applied only to factors with 2 or more
#' levels". This function helps trouble shoot this error when it appears by
#' helping to find the problematic errors. Amazing details are found in the
#' StackOverflow where this function comes from.
#' \href{https://stackoverflow.com/questions/44200195/how-to-debug-contrasts-can-be-applied-only-to-factors-with-2-or-more-levels-er}{Link
#' to the awesome post}
#'
#' The function produces a warning, if there are no complete cases or no factor
#' variables to summarize.
#'
#' @param dat data frame passed to lm or glm via data argument;
#' @param subset_vec the index vector passed to lm or glm via subset argument.
#'
#' @importFrom tibble enframe
#'
#' @return
#' `debug_contrast_error` returns a list with:
#' \itemize{
#'   \item n_levels: A tibble that gives the number of factor levels for all factor variables.
#'   \item levels: A list that gives levels for all factor variables.
#' }
#'
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #### Example 1 --------------------------------
#' dat <- data.frame(y = 1:4,
#'                   x = c(1:3, NA),
#'                   f1 = gl(2, 2, labels = letters[1:2]),
#'                   f2 = c("A", "A", "A", "B"),
#'                   stringsAsFactors = FALSE)
#'
#' dat
#' str(dat)
#'
#' lm(y ~ x + f1 + f2, dat)
#'
#' # Good, we see an error. Now debug_contrast_error exposes that f2 ends up with a
#' # single level.
#' debug_contrast_error(dat = dat)
#'
#' #### Example 2 --------------------------------
#' # An example with a matrix variable x
#' dat <- data.frame(X = I(rbind(matrix(1:6, 3), NA)),
#'                   f = c("a", "a", "a", "b"),
#'                   y = 1:4)
#'
#' dat
#' str(dat)
#'
#' lm(y ~ X + f, data = dat)
#' #Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) :
#' #  contrasts can be applied only to factors with 2 or more levels
#'
#' debug_contrast_error(dat)
#'
#' #### Example 3 --------------------------------
#' # A factor variable with no levels can cause an "contrasts error", too
#' dat <- data.frame(y = 1:4,
#'                   x = rep(NA_real_, 4),
#'                   f1 = gl(2, 2, labels = letters[1:2]),
#'                   f2 = c("A", "A", "A", "B"),
#'                   stringsAsFactors = FALSE)
#' dat
#' str(dat)
#'
#' lm(y ~ x + f1 + f2, dat)
#' #Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) :
#' #  contrasts can be applied only to factors with 2 or more levels
#'
#' debug_contrast_error(dat)
#' }


debug_contrast_error <- function(dat, subset_vec = NULL) {

  if (!is.null(subset_vec)) {

    ## Step 0: explicit subsetting ----------------
    # If you've used the subset argument of lm or glm, start by an explicit
    # subsetting:
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
      }
      subset_log_vec <- subset_vec

    } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
      } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
      }
    } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
    }
    dat <- base::subset(dat, subset = subset_log_vec)

  } else {

    ## Step 1: remove incomplete cases ----------------
    # You can skip this step if you've gone through step 0, since subset
    # automatically removes incomplete cases.
    dat <- stats::na.omit(dat)
  }


  if (nrow(dat) == 0L) {
    warning("no complete cases")
  }


  ## Step 2: mode checking and conversion ----------------
  # Our checking is to produce error when
  #   a "complex" or "raw" is found;
  #   a "logical" or "character" matrix variable is found;
  #
  # and proceed to convert "logical" and "character" to "numeric" of "factor"
  # class. Note that if a data frame column is already a factor variable, it
  # will not be included in ind1, as a factor variable has "numeric" mode

  var_mode <- sapply(dat, mode)

  if (any(var_mode %in% c("complex", "raw"))) {
    stop("complex or raw not allowed!")
  }

  var_class <- sapply(dat, class)

  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    stop("matrix variables with 'AsIs' class must be 'numeric'")
  }

  ind1 <- which(var_mode %in% c("logical", "character"))

  dat[ind1] <- lapply(dat[ind1], as.factor)


  ## Step 3: drop unused factor levels ----------------
  # We won't have unused factor levels for factor variables converted from step
  # 2, i.e., those indexed by ind1. However, factor variables that come with dat
  # might have unused levels (often as the result of step 0 and step 1). We need
  # to drop any possible unused levels from them.

  fctr <- which(sapply(dat, is.factor))

  if (length(fctr) == 0L) {
    warning("no factor variables to summary")
  }

  if (length(ind1) > 0L) {
    ind2 <- fctr[-ind1]
  } else {
      ind2 <- fctr
  }

  dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)

  ## step 4: summarize factor variables ----------------
  # Now we are ready to see what and how many factor levels are actually used by
  # lm or glm:

  lev <- lapply(dat[fctr], base::levels.default)
  nl <- lengths(lev)

  ## Return a list ----------------

  list(n_levels = tibble::enframe(nl,
                                 name = "term",
                                 value = "n_levels"),
       levels = lev)

}





#' @rdname debug_contrast_error
#' @title debug_contrast_error2
#'
#' @description
#' Note: this function relies on `debug_contrast_error`
#'
#' @param form model formula
#'
#' @importFrom tibble as_tibble
#'
#' @return
#' `debug_contrast_error2` returns a list with:
#' \itemize{
#'   \item model_frame: A tibble that gives the model frame (with the "terms" attribute dropped).
#'   \item n_levels: A tibble that gives the number of factor levels for all factor variables.
#'   \item levels: A list that gives levels for all factor variables.
#' }
#' @export
#'
#' @examples
#' #### Example 1 --------------------------------
#' dat <- data.frame(y = 1:4, x = c(1:3, -1), f = rep(letters[1:2], c(3, 1)))
#' debug_contrast_error2(y ~ log(x) + f, dat)
#'
#' # With subset_vec
#' debug_contrast_error2(y ~ log(x) + f, dat, subset_vec = c(1,3,4))
debug_contrast_error2 <- function(form,
                               dat,
                               subset_vec = NULL) {
  ## step 0
  if (!is.null(subset_vec)) {
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
      }
      subset_log_vec <- subset_vec
    } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
      } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
      }
    } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
    }
    dat <- base::subset(dat, subset = subset_log_vec)
  }

  ## step 0 and 1
  dat_internal <- stats::lm(form, data = dat, method = "model.frame")
  attr(dat_internal, "terms") <- NULL

  ## rely on `debug_contrast_error` for steps 2 to 4
  c(list(model_frame = tibble::as_tibble(dat_internal)),
    debug_contrast_error(dat_internal, NULL))
}

