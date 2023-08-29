test_that("Execution works with Eunomia", {
  # First construct a cohort definition set: an empty
  # data frame with the cohorts to generate
  cohortDefinitionSet <- CohortGenerator::createEmptyCohortDefinitionSet()

  # Fill the cohort set using cohorts included in the Eunomia
  # package as an example
  eunomiaCohortSettings <- CohortGenerator::readCsv(system.file("settings/CohortsToCreate.csv", package = "Eunomia"),
    warnOnCaseMismatch = FALSE
  )
  for (i in 1:nrow(eunomiaCohortSettings)) {
    cohortName <- eunomiaCohortSettings$name[i]
    cohortSql <- SqlRender::readSql(sourceFile = system.file(file.path("sql/sql_server", paste0(cohortName, ".sql")), package = "Eunomia"))
    cohortDefinitionSet <- rbind(cohortDefinitionSet, data.frame(
      cohortId = i,
      cohortName = cohortName,
      sql = cohortSql,
      stringsAsFactors = FALSE
    ))
  }

  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  Eunomia::createCohorts(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    cohortTable = "cohort"
  )

  outputFolder <- tempfile()
  cohortTimeSeriesArgs <- createCohortTimeSeriesArgs()

  # Create the time series analyses
  segArgs <- createSegmentedArgs(modelType = "poisson")
  segArgs2 <- createSegmentedArgs(
    modelType = "linear",
    psi = list(id = NA),
    npsi = 1,
    control = segmented::seg.control(
      fix.npsi = FALSE,
      n.boot = 100,
      tol = 1e-7,
      it.max = 100,
      K = 1,
      display = TRUE
    )
  )
  # Now use the ocp default settings
  ocpArgs <- TimeSeriesAnalysis::createOcpArgs()

  # Create the full set of analyses
  tsAnalysis1 <- createTsAnalysis(
    analysisId = 1,
    description = "Poisson model, single change point",
    tsArgs = segArgs
  )
  tsAnalysis2 <- createTsAnalysis(
    analysisId = 2,
    description = "Linear model, single change point, more involved control settings",
    tsArgs = segArgs2
  )
  tsAnalysis3 <- createTsAnalysis(
    analysisId = 3,
    description = "Bayesian Online Changepoint model",
    tsArgs = ocpArgs
  )

  tsAnalysisList <- list(tsAnalysis1, tsAnalysis2, tsAnalysis3)

  executeTimeSeriesAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    cohortTable = "cohort",
    outputFolder = outputFolder,
    databaseId = "Eunomia",
    cohortDefinitionSet = cohortDefinitionSet,
    cohortTimeSeriesArgs = cohortTimeSeriesArgs,
    tsAnalysisList = tsAnalysisList
  )

  # Verify the contents are written to the file system
  dirList <- list.dirs(outputFolder, recursive = TRUE)

  expect_equal(length(dirList), length(tsAnalysisList) + 1) # +1 for the root dir
})
