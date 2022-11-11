#' Run the time series analyses for the selected time series data set
#'
#' @details
#' This function will run the list of time series analyses for the time series
#' data and save the output to the outputFolder.
#'
#' @param tsData   A vector representing the time series data
#' @param tsDataId	An integer value to uniquely identify the time series data. Most often this value will be the cohort ID
#'                  that represents the cohort time series data.
#' @param outputFolder The folder where the time series models are saved
#' @param tsAnalysisList A list of time series analyses as specified by using the @seealso [createSegmentedArgs] and/or @seealso [createOcpArgs]
#'
#' @export
runTsAnalyses <- function(tsData,
                          tsDataId,
                          outputFolder = "./TimeSeriesAnalysis",
                          tsAnalysisList) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assert_vector(tsData, add = errorMessages)
  checkmate::assert_int(tsDataId, add=errorMessages)
  checkmate::assertCharacter(outputFolder, len = 1, add = errorMessages)
  checkmate::assertList(tsAnalysisList, min.len = 1, add = errorMessages)
  
  for (i in 1:length(tsAnalysisList)) {
    checkmate::assertClass(tsAnalysisList[[i]], "tsAnalysis", add = errorMessages)
  }
  
  analysisIds <- unlist(ParallelLogger::selectFromList(tsAnalysisList, "analysisId"))
  uniqueAnalysisIds <- unique(analysisIds)
  if (length(uniqueAnalysisIds) != length(analysisIds)) {
    stop("Duplicate analysis IDs are not allowed")
  }
  
  # Create the output folder
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }
  saveRDS(tsAnalysisList, file.path(outputFolder, "tsAnalysisList.rds"))
  
  message("Building time series models")
  for (i in 1:length(tsAnalysisList)) {
    analysis <- tsAnalysisList[[i]]
    message(paste0("Analysis ", analysis$analysisId, ": ", analysis$description))
    analysisFolder <- file.path(outputFolder, paste0("Analysis", i))
    if (!dir.exists(analysisFolder)) {
      dir.create(analysisFolder)
    }
    
    tsArgs <- tsAnalysisList[[i]]$tsArgs
    if ("SegmentedArgs" %in% class(tsArgs)) {
      model <- createSegmentedModel(tsData = tsData,
                                    segmentedArgs = tsArgs)
    } else if ("OcpArgs" %in% class(tsArgs)) {
      model <- createOcpModel(tsData = tsData,
                              ocpArgs = tsArgs)
    } else {
      stop(paste0("An unknown time series arguments found: ", class(tsArgs), ". Stopping the execution."))
    }
    saveRDS(model, file.path(analysisFolder, .createTimeSeriesRdsFileName(tsDataId = tsDataId)))
  }
}

.createTimeSeriesRdsFileName <- function(tsDataId) {
  invisible(sprintf("ts_d%d.rds", tsDataId))
}