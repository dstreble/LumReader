#' ShinyLumReader application
#'
#' This function calls the 'shinyLumReader' application.
#'
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @examples
#' \dontrun{
#' call_ShinyLumReader()
#' }
#' @export call_ShinyLumReader

call_ShinyLumReader <- function(){

  appDir <- system.file("shinyLumReader", package = "LumReader")

  shiny::runApp(appDir,display.mode = 'normal')

}
