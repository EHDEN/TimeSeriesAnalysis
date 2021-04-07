#' @export
extractTimeSeriesData <- function(connectionDetails = NULL,
                                  connection = NULL,
                                  cdmDatabaseSchema,
                                  tempEmulationSchema = NULL,
                                  cohortDatabaseSchema,
                                  cohortTable = "cohort",
                                  exportFolder,
                                  databaseId,
                                  databaseName = databaseId,
                                  databaseDescription = "",
                                  minCellCount = 5) {
  start <- Sys.time()
  
  # Startup Checks -----
  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  # Counting cohorts -----------------------------------------------------------------------
  ParallelLogger::logInfo("Counting cohorts")
  counts <- getCohortCounts(connection = connection,
                            cohortDatabaseSchema = cohortDatabaseSchema,
                            cohortTable = cohortTable)
  if (nrow(counts) > 0) {
    counts$databaseId <- databaseId
    counts <- enforceMinCellValue(counts, "cohortEntries", minCellCount)
    counts <- enforceMinCellValue(counts, "cohortSubjects", minCellCount)
  } else {
    ParallelLogger::logWarn("No cohort counts found. Did you generate the cohorts?")
  }
  writeToCsv(counts, file.path(exportFolder, "cohort_count.csv"))
  
  
  # Count cohorts by month & year -----------------------------------------------------------------------
  ParallelLogger::logInfo("Count cohorts by month and year")
  cohortCountsByMonthYear <- getCohortCountsByMonthYear(connection = connection,
                                                        cohortDatabaseSchema = cohortDatabaseSchema,
                                                        cohortTable = cohortTable)
  if (nrow(cohortCountsByMonthYear) > 0) {
    cohortCountsByMonthYear$databaseId <- databaseId
    cohortCountsByMonthYear <- enforceMinCellValue(cohortCountsByMonthYear, "eventCount", minCellCount)
    cohortCountsByMonthYear <- enforceMinCellValue(cohortCountsByMonthYear, "subjectCount", minCellCount)
  } else {
    ParallelLogger::logWarn("No cohort counts by month and year found. Did you generate the cohorts?")
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
  
  # Save the database metadata --------------
  ParallelLogger::logInfo("Saving database metadata")
  op <- getObservationPeriodDateRange(connection,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      tempEmulationSchema = tempEmulationSchema)
  database <- data.frame(databaseId = databaseId,
                         databaseName = databaseName,
                         description = databaseDescription,
                         vocabularyVersion = getVocabularyInfo(connection = connection,
                                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                                               tempEmulationSchema = tempEmulationSchema),
                         minObservationPeriodDate = op$minObservationPeriodDate,
                         maxObservationPeriodDate = op$maxObservationPeriodDate,
                         personCount = getPersonCount(connection = connection,
                                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                                      tempEmulationSchema = tempEmulationSchema),
                         isMetaAnalysis = 0)
  writeToCsv(database, file.path(exportFolder, "database.csv"))
  
  # Save package metadata ---------------------------------------------------------------
  ParallelLogger::logInfo("Saving package metadata")
  packageVersionNumber <- packageVersion(getThisPackageName())
  packageMetadata <- data.frame(packageId = getThisPackageName(),
                                packageVersion = packageVersionNumber,
                                executionDate = start,
                                params = as.character(jsonlite::toJSON(list(minCellCount = minCellCount))))
  writeToCsv(packageMetadata, file.path(exportFolder, "package.csv"))
  
  # Export to zip file -------------------------------------------------------------------------------
  zipResults(exportFolder, databaseId)
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Extracting time series data took",
                                signif(delta, 3),
                                attr(delta, "units")))  
}

#' @export
zipResults <- function(exportFolder, databaseId) {
  zipName <- file.path(exportFolder, paste0("Results_", databaseId, ".zip"))
  files <- list.files(exportFolder, pattern = ".*\\.csv$")
  files <- c(files, list.files(exportFolder, pattern = ".*\\.txt$"))
  oldWd <- setwd(exportFolder)
  on.exit(setwd(oldWd), add = TRUE)
  DatabaseConnector::createZipFile(zipFile = zipName, files = files)
  return(zipName)
}

getCohortCounts <- function(connectionDetails = NULL,
                            connection = NULL,
                            cohortDatabaseSchema,
                            cohortTable = "cohort",
                            cohortIds = c()) {
  start <- Sys.time()
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CohortCounts.sql",
                                           packageName = getThisPackageName(),
                                           dbms = connection@dbms,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable,
                                           cohort_ids = cohortIds)
  counts <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Counting cohorts took",
                                signif(delta, 3),
                                attr(delta, "units")))
  return(counts)
  
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