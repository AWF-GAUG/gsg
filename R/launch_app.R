#' Launches the gsg shiny app
#'
#' @export launch_app
#'
#' @return shiny application object
#'
#' @examples
#' # Use launch_app() to start the shiny app
#' launch_app()
launch_app <- function() {
  shiny::shinyApp(ui = shiny_app_ui, server = shiny_app_server)
}