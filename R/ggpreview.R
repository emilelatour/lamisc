

#' @title
#' Preview a file that would be created by ggsave()
#'
#' @description
#' Preview ggsave output in system image viewer. Credit for this one goes to TJ Mahr.
#'
#' @param ... Arguments passed to `ggplot2::ggsave()`
#'
#' @return
#' An image in default system image viewer
#'
#' @references
#' \url{https://gist.github.com/tjmahr/1dd36d78ecb3cff10baf01817a56e895}
#'
#' @export
#'
#' @importFrom ggplot2 ggsave
#'
#' @examples \dontrun{
#' ggplot(mtcars) +
#'   aes(x = wt, y = mpg) +
#'   geom_point()
#'
#' ggpreview(width = 3, height = 3, units = "in")
#'
#' }
ggpreview <- function(...) {
  fname <- base::tempfile(fileext = ".png")
  ggplot2::ggsave(filename = fname, ...)
  base::system2("open", fname)
  base::invisible(NULL)
}

