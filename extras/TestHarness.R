library(TimeSeriesAnalysis)
library(dplyr)

# Load example data from package
data("drugData")

# Create the segmented model args
segArgs <- createSegmentedArgs(modelType = "poisson",
                               psi = 4,
                               npsi = 1)

# Subset to a single cohort
data <- drugData[drugData$cohortDefinitionId == 1, ]

# Add the row_number ID for the ordering - this will
# be done automatically in the pipeline but I may add
# this to the package data object
data <- data %>% mutate(id = row_number())

# Create the segmented model
# m <- createSegmentedModel(tsData = data,
#                           segmentedArgs = segArgs)
#summary(m)

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
# Create the segmented model
# m2 <- createSegmentedModel(tsData = data,
#                            segmentedArgs = segArgs2)
# 
#summary(m2)

# Now use the ocp default settings
ocpArgs <- TimeSeriesAnalysis::createOcpArgs()

# Now use the analysis approach
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

#debugonce(runTsAnalyses)

# # TODO - Incorporate the logic for extracting time series data from cohort table
# # and save it for use in the modeling
# message("Extracting time series data....down the road.")
# # For now use the drug data example data
# data("drugData")
# data <- drugData
# cohortIds <- unique(data$cohortDefinitionId)

runTsAnalyses(tsData = data$eventCount,
              tsDataId = 1,
              outputFolder = "E:/Timeseries/",
              tsAnalysisList = tsAnalysisList)

ParallelLogger::saveSettingsToJson(tsAnalysisList, fileName = "E:/Timeseries/specs.json")

ocpModel1 <- readRDS(file = "E:/Timeseries/Analysis3/ts_d1.rds")
