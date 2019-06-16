#' Run the neurorct built-in shiny app
#'
#' @return run a Shiny app
#' @export
#'
#' @examples
run_shiny <- function() {
  appDir <- system.file("shiny_apps", "myapp", package = "neurorct")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `neurorct`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
