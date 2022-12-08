#' Extracts the time series data and runs the time series analyses on all 
#' (or a subset of) the cohorts. This function assumes the cohorts have 
#' already been generated using the OHDSI CohortGenerator package 
#'
#' @details
#' The time series data for each cohort is extracted and stored to the file system.
#' Then each cohort's time series is used to build one or more models. 
#'
#' @template Connection
#'
#' @template CdmDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @template CohortDatabaseSchema
#'
#' @template CohortTable
#'
#' @param outputFolder The location where the cohort time series data results will be written.
#' 
#' @param databaseId The database identifier for the time series data
#' 
#' @param cohortDefinitionSet The cohort definition set for the cohorts used for the analysis
#' 
#' @param tsDataFieldName The time series data field name to use. This will be either the "subjectCount" or "eventCount" as 
#'                        computed based on the cohort.
#'
#' @param cohortTimeSeriesArgs The cohort time series arguments. @seealso[createCohortTimeSeriesArgs] for more information.
#' 
#' @param tsAnalysisList A list of time series analyses as specified by using the @seealso [createSegmentedArgs] and/or @seealso [createOcpArgs]
#' 
#' @export
executeTimeSeriesAnalyses <- function(connectionDetails = NULL,
                                      connection = NULL,
                                      cdmDatabaseSchema,
                                      tempEmulationSchema = NULL,
                                      cohortDatabaseSchema,
                                      cohortTable = "cohort",
                                      outputFolder,
                                      databaseId,
                                      cohortDefinitionSet,
                                      tsDataFieldName = "subjectCount",
                                      cohortTimeSeriesArgs,
                                      tsAnalysisList) {
  checkmate::assert_choice(tsDataFieldName, choices = c("subjectCount", "eventCount"))
  
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  
  if (!dir.exists(outputFolder)) {
    dir.create(path = outputFolder, recursive =TRUE)
  }
  
  # Save the cohorts
  CohortGenerator::writeCsv(x = cohortDefinitionSet,
                            file = file.path(outputFolder, "cohort.csv"))
  
  # Get the cohort counts for the cohorts of interest
  cohortIds <- cohortTimeSeriesArgs$cohortIds
  if (cohortIds == -1) {
    cohortIds = c()
  }
  cohortCounts <- CohortGenerator::getCohortCounts(connection = connection,
                                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                                   cohortTable = cohortTable,
                                                   cohortIds = cohortIds,
                                                   databaseId = databaseId)
  CohortGenerator::writeCsv(x = cohortCounts,
                            file = file.path(outputFolder, "cohort_count.csv"))

  # Get database information
  .createDatabaseMetaData(connection = connection,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outputFolder = outputFolder,
                          databaseId = databaseId)

  # Get the time series data for the cohorts of interest
  tsData <- getCohortTimeSeriesData(connectionDetails = connectionDetails,
                                    connection = connection,
                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                    tempEmulationSchema = tempEmulationSchema,
                                    cohortDatabaseSchema = cohortDatabaseSchema,
                                    cohortTable = cohortTable,
                                    outputFolder = outputFolder,
                                    databaseId = databaseId,  
                                    cohortTimeSeriesArgs = cohortTimeSeriesArgs)
  
  # Iterate over the tsData results and run the time series analyses
  # for each of the data sets
  cohortIds <- unique(tsData$cohortDefinitionId)
  for(i in 1:length(cohortIds)) {
    tsDataSubset <- tsData %>%
      filter(.data$cohortDefinitionId == cohortIds[i]) %>%
      arrange(.data$cohortStartDate) %>%
      select(.data$cohortStartDate, tsDataFieldName) %>%
      rename(eventDate = .data$cohortStartDate,
             eventCount = tsDataFieldName)
    rlang::inform("---------------------------------")
    rlang::inform(paste0("Fitting model for cohort ", i, " of ", length(cohortIds)))
    rlang::inform("---------------------------------")
    runTsAnalyses(tsData = tsDataSubset,
                  tsDataId = cohortIds[i],
                  outputFolder = outputFolder,
                  tsAnalysisList = tsAnalysisList)
  }
}

.createDatabaseMetaData <- function(connection, cdmDatabaseSchema, outputFolder, databaseId) {
  sql <- "SELECT TOP 1 * FROM @cdm_database_schema.cdm_source;"
  cdmSource <- renderTranslateQuerySql(connection = connection,
                                       sql = sql,
                                       snakeCaseToCamelCase = TRUE,
                                       cdm_database_schema = cdmDatabaseSchema)
  
  sql <- "SELECT TOP 1 vocabulary_version  FROM @cdm_database_schema.vocabulary WHERE vocabulary_id = 'None';"
  vocabVersion <- renderTranslateQuerySql(connection = connection,
                                          sql = sql,
                                          snakeCaseToCamelCase = TRUE,
                                          cdm_database_schema = cdmDatabaseSchema)
  
  sql <- "SELECT MAX(observation_period_end_date) as max_obs_period_end_date
  FROM @cdm_database_schema.observation_period;"
  observationPeriodMax <- renderTranslateQuerySql(connection = connection,
                                                  sql = sql,
                                                  snakeCaseToCamelCase = TRUE,
                                                  cdm_database_schema = cdmDatabaseSchema)
  
  database <- cdmSource %>%
    mutate(vocabularyVersion = vocabVersion$vocabularyVersion,
           databaseId = !!databaseId) %>%
    bind_cols(observationPeriodMax)
  
  CohortGenerator::writeCsv(x = database,
                            file = file.path(outputFolder, "database.csv"))
}