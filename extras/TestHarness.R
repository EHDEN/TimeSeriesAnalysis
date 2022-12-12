library(TimeSeriesAnalysis)
library(dplyr)
library(Eunomia)

# Export folder
outputFolder <- "E:/Timeseries/"


getEunomiaTimeSeriesData <- function() {
  # Use Eunomia to generate a few cohorts
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  Eunomia::createCohorts(connectionDetails = connectionDetails)
  
  # Get the time series data
  cohortTimeSeriesArgs <- TimeSeriesAnalysis::createCohortTimeSeriesArgs()
  TimeSeriesAnalysis::getCohortTimeSeriesData(connectionDetails = connectionDetails,
                                              cdmDatabaseSchema = "main",
                                              cohortDatabaseSchema = "main",
                                              cohortTable = "cohort",
                                              outputFolder = outputFolder,
                                              cohortTimeSeriesArgs = cohortTimeSeriesArgs,
                                              databaseId = "Eunomia")
  
  data <- CohortGenerator::readCsv(file = file.path(outputFolder, "cohort_time_series_data.csv"))
  invisible(data)
}

sara.plot <- function(o.seg, tsData2) {
  # Code From Sara
  plot(o.seg, conf.level=0.95, shade=TRUE, type = "l", xlab = "Time", ylab = "N Drugs", main = paste("Segmented Linear Regression, npsi = 1"))# plots regression lines of the two segments using the coeffs returned in o.seg
  points(tsData2$eventDate,tsData2$eventCount, xlab = "Event Date", ylab = "Event Count", cex= 1.5, pch=16)# add the actual time series, ,type = "l"
  #debugonce(segmented::plot.segmented)
  plot(o.seg,add=TRUE,link=FALSE,lwd=2,col=3:5, lty=c(1,1)) # added fitted line using diff cols for diff segments
  #debugonce(segmented::points.segmented)
  points(o.seg, link=TRUE, col=2)# circle the brkpt
  lines(o.seg,col=2,pch=19,bottom=FALSE,lwd=2) # add CI for the breakpoint
  cpts_segmented = o.seg$psi[2] # the final brkpt
  for(cp in 1:length(cpts_segmented)){
    abline(v= cpts_segmented[cp], col = "red", lwd= 3,lty=2)}
}

# TODO: Provide working example using other cohorts outside of Eunomia
# https://ohdsi.github.io/CohortGenerator/articles/GeneratingCohorts.html

data(drugData)

data <- drugData

tsData <- data %>%
  filter(cohortDefinitionId == 1) %>%
  arrange(cohortStartDate) %>%
  select(cohortStartDate, subjectCount) %>%
  rename(eventDate = cohortStartDate,
         eventCount = subjectCount)

tsData2 <- data %>%
  filter(cohortDefinitionId == 2) %>%
  arrange(cohortStartDate) %>%
  select(cohortStartDate, subjectCount) %>%
  rename(eventDate = cohortStartDate,
         eventCount = subjectCount)

# Create the segmented model args
segArgs <- createSegmentedArgs(modelType = "poisson")


# Doing this with another set of parameters
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

# Run the TS analyses
runTsAnalyses(tsData = tsData,
              tsDataId = 1,
              outputFolder = outputFolder,
              tsAnalysisList = tsAnalysisList)

# Save the settings used to build the models
ParallelLogger::saveSettingsToJson(tsAnalysisList, fileName = file.path(outputFolder, "specs.json"))

# Inspect the models
m <- readRDS(file = "E:/Timeseries/Analysis1/ts_d1.rds")
TimeSeriesAnalysis::plotSegmented(m$tsData, m$model)
sara.plot(m$model, m$tsData)

m2 <- readRDS(file = "E:/Timeseries/Analysis2/ts_d1.rds")
TimeSeriesAnalysis::plotSegmented(m2$tsData, m2$model)
sara.plot(m2$model, m2$tsData)

m3 <- readRDS(file = "E:/Timeseries/Analysis3/ts_d1.rds")
#debugonce(TimeSeriesAnalysis::plotOcp)
TimeSeriesAnalysis::plotOcp(m3$tsData, m3$model)

