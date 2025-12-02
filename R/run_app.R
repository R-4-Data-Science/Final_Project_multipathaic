#' Run the multipathaic Shiny Web Application
#'
#' Launches the full multipathaic dashboard.
#'
#' @export
run_multipathaic_app <- function() {
  app_dir <- system.file("app", package = "multipathaic")
  
  if (app_dir == "") {
    stop("App directory not found. Make sure inst/app/app.R exists.")
  }
  
  shiny::runApp(app_dir, launch.browser = TRUE)
}
