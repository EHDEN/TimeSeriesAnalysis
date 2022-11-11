#' Create a TimeSeries analysis specification
#'
#' @details
#' Create a set of analysis choices, to be used with the [runTsAnalyses()] function.
#'
#' Providing a NULL value for any of the argument applies the corresponding step will not be executed.
#'
#' @param analysisId    An integer that will be used later to refer to this specific
#'                      set of analysis choices.
#' @param description   A short description of the analysis.
#' @param tsArgs        An object representing the time series arguments to be used 
#'                      when calling the various time series model functions
#'
#' @export
createTsAnalysis <- function(analysisId = 1,
                             description = "",
                             tsArgs) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(analysisId, add = errorMessages)
  checkmate::assertCharacter(description, len = 1, add = errorMessages)
  checkmate::assert_class(x = tsArgs, classes = c("SegmentedArgs", "OcpArgs"), add = errorMessages)

  analysis <- list()
  for (name in names(formals(createTsAnalysis))) {
    analysis[[name]] <- get(name)
  }
  
  class(analysis) <- "tsAnalysis"
  return(analysis)
}