#' @export
runStudy <- function(connectionDetails = NULL,
                     connection = NULL,
                     cdmDatabaseSchema,
                     tempEmulationSchema = NULL,
                     cohortDatabaseSchema,
                     cohortTable = "cohort",
                     exportFolder,
                     incremental = TRUE,
                     databaseId,
                     databaseName = databaseId,
                     databaseDescription = "",
                     changePointMonth,
                     changePointYear,
                     minCellCount = 5) {
  start <- Sys.time()
  if (!is.numeric(changePointMonth) || !is.numeric(changePointYear)) {
    stop("changePointMonth and changePointYear must contain an integer value.")
  }

  # Setup logging ---------
  ParallelLogger::clearLoggers()  # Ensure that any/all previous logging activities are cleared
  ParallelLogger::addDefaultFileLogger(file.path(exportFolder,
                                                 paste0(getThisPackageName(), ".txt")))
  ParallelLogger::addDefaultErrorReportLogger(file.path(exportFolder, paste0(getThisPackageName(),
                                                                             "ErrorReportR.txt")))
  ParallelLogger::addDefaultConsoleLogger()
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_CONSOLE_LOGGER", silent = TRUE), add = TRUE)

  # Write out the system information
  ParallelLogger::logInfo(.systemInfo())

  # Create the cohorts
  output <- createCohorts(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortTable = cohortTable,
                          incremental = incremental,
                          incrementalFolder = file.path(exportFolder, "RecordKeeping"))

  # Extract the results
  extractTimeSeriesData(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        tempEmulationSchema = tempEmulationSchema,
                        cohortDatabaseSchema = cohortDatabaseSchema,
                        cohortTable = cohortTable,
                        exportFolder = exportFolder,
                        databaseId = databaseId,
                        databaseName = databaseName,
                        databaseDescription = databaseDescription,
                        minCellCount = minCellCount)

  # Build the models
  buildModels(exportFolder = exportFolder,
              changePointMonth = changePointMonth,
              changePointYear = changePointYear)

  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Running the study took", signif(delta, 3), attr(delta, "units")))
}
