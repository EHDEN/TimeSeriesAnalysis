#' Launches the Shiny results viewer
#'
#' @description
#' This function launches the Shiny web results viewer.
#'
#' @param outputFolder The root output folder that holds the individual database results
#' @param shinySettings This is a list() of settings passed to the Shiny application. At
#' the moment this is simply the output folder so likely no need to change this parameter.
#'
#' @export
launchShinyApp <- function(outputFolder, shinySettings = list(
                             dataFolder = outputFolder
                           )) {
  appDir <- system.file("shiny/ResultsExplorer", package = "TimeSeriesAnalysis", mustWork = TRUE)
  .GlobalEnv$shinySettings <- shinySettings
  shiny::runApp(appDir)
}
