#' Run the neurorct built-in shiny app
#'
#' @return run a Shiny app
#' @export
#'
#' @examples
#' library(neurorct)
#' neurorct_gui()
#'
#'
neurorct_gui <- function() {
  appDir <- system.file("shiny_apps", "myapp", package = "neurorct")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `neurorct`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
