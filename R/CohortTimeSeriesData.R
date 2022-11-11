#' Create a parameter object for the function getCohortTimeSeriesData
#'
#' @details
#' Create an object defining the parameter values for use with the 
#' @seealso [getCohortTimeSeriesData]
#'
#' @param cohortIds  A vector of cohort Ids; leave empty to use all cohorts in the cohort table
#' @param timeUnits  The time units (year, month, day) to use when extracting the cohort time series.
#' @param sortBy  	 The value from the cohort table to sort by: cohortStartDate or cohortEndDate
#'
#' @export
createCohortTimeSeriesArgs <- function(cohortIds = c(),
                                       timeUnits = "month",
                                       sortBy = "cohortStartDate") {
  analysis <- list()
  for (name in names(formals(createCohortTimeSeriesArgs))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- c("cohortTimeSeriesArgs", "args")
  return(analysis)
}

#' Retrieve time series data for one or more cohorts
#' 
#' @details 
#' This function will retrieve a data frame representing
#' entry events for one or more cohorts grouped into time intervals
#' as specified by the cohortTimeSeriesArgs
#' 
#' 
#' @export
getCohortTimeSeriesData <- function(connectionDetails = NULL,
                              connection = NULL,
                              cdmDatabaseSchema,
                              tempEmulationSchema = NULL,
                              cohortDatabaseSchema,
                              cohortTable = "cohort",
                              exportFolder,
                              cohortTimeSeriesArgs) {
  start <- Sys.time()

  # Startup Checks -----
  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  # Extract the time series information  -----------------------------------------------------
  message("Get time series data")
  cohortCountsByMonthYear <- getCohortCountsByMonthYear(connection = connection,
                                                        cohortDatabaseSchema = cohortDatabaseSchema,
                                                        cohortTable = cohortTable)
  if (nrow(cohortCountsByMonthYear) <= 0) {
    warning("No cohort counts by month and year found. Did you generate the cohorts?")
  }
  writeToCsv(cohortCountsByMonthYear, file.path(exportFolder, "cohort_count_by_month_year.csv"))

  # Extract the secular trend data for each target/event combination -------
  trendsByTargetEvent <- createAndGetTrendsByTargetEvent(connection = connection,
                                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                                         cohortTable = cohortTable,
                                                         tempEmulationSchema = tempEmulationSchema)
  if (nrow(trendsByTargetEvent) > 0) {
    trendsByTargetEvent$databaseId <- databaseId
    trendsByTargetEvent <- enforceMinCellValue(trendsByTargetEvent, "eventCount", minCellCount)
  } else {
    ParallelLogger::logWarn("No trends by month and year found. Did you generate the cohorts?")
  }
  writeToCsv(trendsByTargetEvent, file.path(exportFolder, getTrendsFileName()))
  
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Time series data extract took", signif(delta, 3), attr(delta, "units")))
}

getCohortCountsByMonthYear <- function(connectionDetails = NULL,
                                       connection = NULL,
                                       cohortDatabaseSchema,
                                       cohortTable = "cohort") {
  start <- Sys.time()

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CohortCountsByMonthYear.sql",
                                           packageName = getThisPackageName(),
                                           dbms = connection@dbms,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable)
  trends <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Get cohorts counts by month and year took",
                                signif(delta, 3),
                                attr(delta, "units")))
  return(trends)
}
