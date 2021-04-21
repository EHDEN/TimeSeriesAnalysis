library(TimeSeriesAnalysis)
# Specify where the temporary files (used by the Andromeda package) will be created:
andromedaTempFolder <- if (Sys.getenv("ANDROMEDA_TEMP_FOLDER") == "") "~/andromedaTemp" else Sys.getenv("ANDROMEDA_TEMP_FOLDER")
options(andromedaTempFolder = andromedaTempFolder)

cdmSchemaList <- c("cdm_premier_v1481", "cdm_optum_ehr_covid_v1547")
resultsSchemaList <- c("scratch_asena5_lsc", "instantiated_cohorts")
dbList <- c("premier", "optum_ehr")
dbNameList <- c("Premier", "Optum EHR")
for (i in 1:length(dbList)) {
  # Details for connecting to the server: database <- 'optum_ehr'
  database <- dbList[i]
  dbms <- Sys.getenv("DBMS")
  user <- Sys.getenv("DB_USER")
  password <- Sys.getenv("DB_PASSWORD")
  server <- paste0(Sys.getenv("DB_SERVER"), "/", database)
  port <- Sys.getenv("DB_PORT")
  # Define a schema that can be used to emulate temp tables:
  tempEmulationSchema <- NULL
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = server,
                                                                  user = user,
                                                                  password = password,
                                                                  port = port)
  
  # Details for connecting to the CDM and storing the results cdmDatabaseSchema
  cdmDatabaseSchema <- cdmSchemaList[i]
  cohortDatabaseSchema <- resultsSchemaList[i]
  cohortTable <- paste0("ags_ts_cohorts_", cdmDatabaseSchema)
  minCellCount <- 5
  
  # Record the database details
  databaseId <- cdmDatabaseSchema
  databaseName <- dbNameList[i]
  databaseDescription <- dbNameList[i]
  
  # Set the folder for holding the study output
  projectRootFolder <- "D:/TimeSeriesAnalysis/Run"
  outputFolder <- file.path(projectRootFolder, databaseId)
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }
  setwd(outputFolder)
  
  runStudy(connectionDetails = connectionDetails,
           cdmDatabaseSchema = cdmDatabaseSchema,
           tempEmulationSchema = tempEmulationSchema,
           cohortDatabaseSchema = cohortDatabaseSchema,
           cohortTable = cohortTable,
           exportFolder = outputFolder,
           databaseId = databaseId,
           databaseName = databaseName,
           databaseDescription = databaseDescription,
           changePointMonth = 3,
           changePointYear = 2020,
           minCellCount = minCellCount)
}


consolidatedResultsFolder <- "D:/TimeSeriesAnalysis/Run/Results"
preMergeResultsFiles(consolidatedResultsFolder)
# Build the models
buildModels(exportFolder = consolidatedResultsFolder, changePointMonth = 3, changePointYear = 2020)
