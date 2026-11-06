#' List the packages used in an R script or Quarto/R Markdown document
#'
#' @description
#' Parses an `.R`, `.qmd`, or `.Rmd` file and reports which packages its code
#' relies on. Three kinds of usage are detected:
#'
#' * **Loading calls**: `library()`, `require()`, `requireNamespace()`,
#'   `loadNamespace()`, and `pacman::p_load()`, whether the package is written
#'   as a bare name (`library(dplyr)`) or a string (`library("dplyr")`), and
#'   wherever the call sits (inside `suppressPackageStartupMessages()`, an
#'   `if` block, a function body, ...).
#' * **Namespaced calls** (`pkg::fun()` or `pkg:::fun()`): picked up directly
#'   from the parse tree.
#' * **Bare calls** (`fun()`): matched against the exports of every package
#'   currently on the search path. A bare call is attributed to a package only
#'   if that package is attached in the *current* R session.
#'
#' The first two are read straight from the file, so they don't depend on
#' what is installed or attached. For `.qmd`/`.Rmd` files, the R code is
#' first extracted with [knitr::purl()] into a temporary `.R` file, which is
#' then parsed.
#'
#' Useful for building a `DESCRIPTION`/`renv` dependency list, writing the
#' "software" paragraph of a methods section, or auditing an analysis script
#' before handing it off.
#'
#' @param file `character(1)`. Path to an `.R`, `.qmd`, or `.Rmd` file. The
#'   extension check for Quarto/R Markdown is case-insensitive (`.qmd`, `.Rmd`,
#'   `.rmd`, ...). Any other extension is parsed as plain R code.
#'
#' @return A tibble with one row per package-by-detection-method, sorted by
#'   `package`, with columns:
#'   \describe{
#'     \item{package}{`character`. Package name.}
#'     \item{how}{`character`. The loading function (`"library"`,
#'       `"require"`, `"requireNamespace"`, `"loadNamespace"`, `"p_load"`)
#'       if the package was loaded explicitly; `"::"` if it was referenced
#'       with a namespace operator; `"bare"` if it was matched from
#'       unqualified function calls.}
#'     \item{functions}{`character`. For `"bare"` rows, a comma-separated,
#'       alphabetized list of the matched function names. `NA` otherwise.}
#'   }
#'   A package can appear on several rows if it is used more than one way.
#'   Use `dplyr::distinct(package)` to get a unique list.
#'
#' @section Caveats:
#' * **Bare-call detection depends on your session, not the file.** It looks
#'   only at packages attached right now (see [search()]). Loading calls in
#'   the file are reported, but they are not used to decide which package a
#'   bare call belongs to. A package you attached for unrelated reasons can
#'   show up if it exports a function name the file happens to call.
#' * **Masking produces duplicates.** If two attached packages export the same
#'   name (e.g. `select()` from dplyr and MASS), the call is attributed to
#'   every attached package that exports it, not just the one that wins on
#'   the search path. Re-exports count too: dplyr re-exports `tibble()`, so
#'   a bare `tibble()` call is credited to dplyr if dplyr is attached.
#' * **Base R packages are excluded from bare matching:** base, stats, utils,
#'   methods, graphics, grDevices, and datasets. They still appear if loaded
#'   explicitly or used with `::` (e.g. `stats::lm()`).
#' * **Computed package names are skipped.** `library(pkg, character.only =
#'   TRUE)` where `pkg` is a variable, or `lapply(pkgs, library, ...)`, can't
#'   be resolved without running the code.
#' * **Also not detected:** functions passed as arguments without being
#'   called (e.g. `purrr::map(x, mean)` finds purrr but not `mean`),
#'   functions called via strings (`do.call("fun", ...)`), inline `` `r ` ``
#'   code in `.qmd`/`.Rmd` files, non-R chunks, and code pulled in with
#'   `source()` or child documents.
#' * **Chunks with `eval = FALSE`** are commented out by [knitr::purl()] and
#'   therefore ignored.
#' * **The file must parse.** A syntax error anywhere in the extracted code
#'   causes [parse()] to fail.
#'
#' @seealso [knitr::purl()], [utils::getParseData()], [search()],
#'   [getNamespaceExports()]. For a more complete dependency scan, see
#'   `renv::dependencies()`.
#'
#' @importFrom utils getParseData
#' @export
#'
#' @examples
#' \dontrun{
#' # Full breakdown, including which bare functions matched each package
#' pkgs_used("code/10_draft-analysis.qmd")
#'
#' # Just the unique package names
#' pkgs_used("code/10_draft-analysis.qmd") |>
#'   dplyr::distinct(package) |>
#'   dplyr::pull()
#' }
#'
#' # Self-contained example with a plain R script
#' tmp <- tempfile(fileext = ".R")
#' writeLines(
#'   c("library(stats4)",
#'     "suppressPackageStartupMessages(require('splines'))",
#'     "x <- tibble::tibble(a = 1:3)",
#'     "y <- dplyr::mutate(x, b = a * 2)",
#'     "summary(y)"),
#'   tmp
#' )
#' pkgs_used(tmp)
#' unlink(tmp)
pkgs_used <- function(file) {

  # Silence note: Undefined global functions or variables
  package <- NULL

  # Pull the R code out of a qmd/Rmd first
  if (grepl("\\.(q|R)md$", file, ignore.case = TRUE)) {
    tmp <- tempfile(fileext = ".R")
    on.exit(unlink(tmp), add = TRUE)
    knitr::purl(file, output = tmp, quiet = TRUE, documentation = 0)
    file <- tmp
  }

  exprs <- parse(file, keep.source = TRUE)
  pd    <- utils::getParseData(exprs)

  # Packages loaded with library(), require(), etc.
  loaded <- .find_loaded_pkgs(exprs)

  # Packages referenced explicitly with pkg::
  ns_pkgs <- unique(pd$text[pd$token == "SYMBOL_PACKAGE"])

  # Bare function calls, matched to whichever attached package exports them
  calls    <- unique(pd$text[pd$token == "SYMBOL_FUNCTION_CALL"])
  attached <- sub("^package:", "", grep("^package:", search(), value = TRUE))
  attached <- setdiff(attached, c("base", "stats", "utils", "methods",
                                  "graphics", "grDevices", "datasets"))

  bare <- purrr::map(attached, \(p) {
    hits <- intersect(calls, getNamespaceExports(p))
    if (length(hits)) tibble::tibble(package = p, how = "bare", functions = toString(sort(hits)))
  }) |>
    purrr::list_rbind()

  dplyr::bind_rows(
    loaded,
    tibble::tibble(package = ns_pkgs, how = "::", functions = NA_character_),
    bare
  ) |>
    dplyr::arrange(package)
}


# Walk the parsed code and pull package names out of loading calls.
# Returns a tibble(package, how, functions = NA) with unique rows.
#
# Note: if bare-call matching should someday stop depending on the session,
# the packages found here are the natural candidates to match against
# (via getNamespaceExports(), for those that are installed).
.find_loaded_pkgs <- function(exprs) {
  # loading function -> does it take several packages (p_load) or one?
  load_fns <- c(library = FALSE, require = FALSE, requireNamespace = FALSE,
                loadNamespace = FALSE, p_load = TRUE)

  found <- list()

  # Accept bare names, or strings; skip names when character.only = TRUE,
  # since then the symbol is a variable holding the name.
  as_pkg <- function(arg, character_only = FALSE) {
    if (is.character(arg) && length(arg) == 1) return(arg)
    if (is.name(arg) && !character_only) return(as.character(arg))
    NULL
  }

  # Recurse into each piece, skipping empty arguments (as in x[, 1]), which
  # can't be held in a variable without erroring
  walk_parts <- function(e) {
    parts <- as.list(e)
    for (i in seq_along(parts)) {
      if (!identical(parts[[i]], quote(expr = ))) walk(parts[[i]])
    }
  }

  walk <- function(e) {
    if (is.call(e)) {
      head <- e[[1]]
      # Handles fun(), pkg::fun(), and pkg:::fun()
      fn <- if (is.name(head)) {
        as.character(head)
      } else if (is.call(head) && length(head) == 3 &&
                 as.character(head[[1]]) %in% c("::", ":::")) {
        as.character(head[[3]])
      }

      if (length(fn) == 1 && fn %in% names(load_fns)) {
        args <- as.list(e)[-1]
        nms  <- if (is.null(names(args))) rep("", length(args)) else names(args)
        char_only <- isTRUE(args[["character.only"]])

        if (load_fns[[fn]]) {
          # p_load(): every unnamed argument is a package
          pkgs <- lapply(args[nms == ""], as_pkg, character_only = char_only)
        } else {
          # Others: the `package` argument, or the first unnamed one
          arg  <- if ("package" %in% nms) args[["package"]] else args[nms == ""][1][[1]]
          pkgs <- list(if (!is.null(arg)) as_pkg(arg, char_only))
        }

        for (p in unlist(pkgs)) found[[length(found) + 1]] <<- c(p, fn)
      }

      walk_parts(e)
    } else if (is.pairlist(e)) {
      # Default argument values in function() definitions
      walk_parts(e)
    }
  }

  for (e in exprs) walk(e)

  if (!length(found)) {
    return(tibble::tibble(package = character(), how = character(),
                          functions = character()))
  }

  m <- do.call(rbind, found)
  unique(tibble::tibble(package = m[, 1], how = m[, 2], functions = NA_character_))
}
