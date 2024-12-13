#' Run the MSdial To Jupyter Application
#'
#' This function launches the Shiny application for converting MSdial output to Jupyter format.
#' The application allows users to upload MSdial CSV output and convert it to a format
#' compatible with Jupyter notebooks.
#'
#' @return A Shiny application object
#' @export
#'
#' @examples
#' if(interactive()){
#'   run_MSdial_to_Jupyter_app()
#' }
run_MSdial_to_Jupyter_app <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
} 