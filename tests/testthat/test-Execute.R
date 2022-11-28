test_that("Execution works with Eunomia", {
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
  segArgs2 <- createSegmentedArgs(modelType = "linear",
                                  psi = list(id = NA),
                                  npsi = 1,
                                  control = segmented::seg.control(fix.npsi=FALSE, 
                                                                   n.boot=100, 
                                                                   tol=1e-7, 
                                                                   it.max = 100, 
                                                                   K=1, 
                                                                   display=TRUE))
  # Now use the ocp default settings
  ocpArgs <- TimeSeriesAnalysis::createOcpArgs()
  
  # Create the full set of analyses
  tsAnalysis1 <- createTsAnalysis(analysisId = 1,
                                  description = "Poisson model, single change point",
                                  tsArgs = segArgs)
  tsAnalysis2 <- createTsAnalysis(analysisId = 2,
                                  description = "Linear model, single change point, more involved control settings",
                                  tsArgs = segArgs2)
  tsAnalysis3 <- createTsAnalysis(analysisId = 3,
                                  description = "Bayesian Online Changepoint model",
                                  tsArgs = ocpArgs)
  
  tsAnalysisList <- list(tsAnalysis1, tsAnalysis2, tsAnalysis3)  
  
  executeTimeSeriesAnalyses(connectionDetails = connectionDetails,
                            cdmDatabaseSchema = "main",
                            cohortDatabaseSchema = "main",
                            cohortTable = "cohort",
                            outputFolder = outputFolder,
                            databaseId = "Eunomia",
                            cohortTimeSeriesArgs = cohortTimeSeriesArgs,
                            tsAnalysisList = tsAnalysisList)
  
  # Verify the contents are written to the file system
  dirList <- list.dirs(outputFolder, recursive = TRUE)
  
  expect_equal(length(dirList), length(tsAnalysisList)+1) # +1 for the root dir
})
