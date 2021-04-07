#' @export
createCohorts <- function(connectionDetails = NULL,
                          connection = NULL,
                          cdmDatabaseSchema,
                          tempEmulationSchema = NULL,
                          cohortDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = "cohort",
                          createCohortTable = TRUE,
                          incremental = TRUE,
                          incrementalFolder = NULL) {
  # Assemble the cohort set for generation
  cohortSet <- CohortGenerator::createEmptyCohortSet()
  cohortsToCreate <- readCsv("settings/cohortsToCreate.csv")
  for (i in 1:nrow(cohortsToCreate)) {
    cohortFullName <- cohortsToCreate$name[i]
    cohortId <- cohortsToCreate$cohortId[i]
    cohortJsonFile <- system.file(paste0("cohorts/", cohortId, ".json"), package = getThisPackageName(), mustWork = TRUE)
    cohortJson <- CohortGenerator::readCirceExpressionJsonFile(cohortJsonFile)
    cohortSqlFile <- system.file(paste0("sql/sql_server/", cohortId, ".sql"), package = getThisPackageName(), mustWork = TRUE)
    cohortSql <- SqlRender::readSql(cohortSqlFile)
    cohortSet <- rbind(cohortSet, data.frame(cohortId = cohortId,
                                             cohortFullName = cohortFullName, 
                                             sql = cohortSql, 
                                             json = cohortJson,
                                             stringsAsFactors = FALSE))
  }  
  
  # Create the cohorts
  results <- CohortGenerator::instantiateCohortSet(connectionDetails = connectionDetails,
                                                   connection = connection,
                                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                                   tempEmulationSchema = tempEmulationSchema,
                                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                                   cohortTable = cohortTable,
                                                   cohortSet = cohortSet,
                                                   createCohortTable = createCohortTable,
                                                   incremental = incremental,
                                                   incrementalFolder = incrementalFolder)
  return(results)
  
}