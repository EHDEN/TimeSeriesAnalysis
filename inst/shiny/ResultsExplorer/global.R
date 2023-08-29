library(dplyr)
library(shiny)
library(DatabaseConnector)

# Borrowed from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
is_installed <- function(pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

dataFolder <- shinySettings$dataFolder

# Verify that the results folder provided is of the right structure
databaseFolders <- list.dirs(path = dataFolder, recursive = FALSE)

if (length(databaseFolders) == 0) {
  stop(paste0("The data folder '", dataFolder, "' must contain subfolders for each database's results."))
}

readResultsFiles <- function(databaseFolders, fileName, operation = "append", includeFolderName = FALSE) {
  results <- NULL
  for (i in 1:length(databaseFolders)) {
    contents <- CohortGenerator::readCsv(
      file <- file.path(databaseFolders[i], fileName)
    )
    if (includeFolderName) {
      contents$folderName <- databaseFolders[i]
    }
    if (i == 1) {
      results <- contents
    } else {
      results <- rbind(results, contents)
      if (operation == "unique") {
        results <- unique(results)
      }
    }
  }
  return(results)
}

readAnalysisListFiles <- function(databaseFolders) {
  analyses <- data.frame()
  for (i in 1:length(databaseFolders)) {
    analysisList <- readRDS(
      file = file.path(databaseFolders[i], "tsAnalysisList.rds")
    )
    for (j in 1:length(analysisList)) {
      r <- data.frame(analysisId = analysisList[[j]]$analysisId,
                      description = analysisList[[j]]$description)
      if (i == 1) {
        analyses <- r    
      } else {
        analyses <- rbind(analyses, r)  
      }
    }
  }
  analyses <- unique(analyses)
  return(
    list(
      analysisList = analysisList, 
      analyses = analyses
    )
  )
}

# Setup the app & read results
cohorts <- readResultsFiles(
  databaseFolders = databaseFolders,
  fileName = "cohort.csv",
  operation = "unique"
)
database <- readResultsFiles(
  databaseFolders = databaseFolders,
  fileName = "database.csv",
  includeFolderName = TRUE
)
cohortCount <- readResultsFiles(
  databaseFolders = databaseFolders,
  fileName = "cohort_count.csv"
)
cohortTimeSeriesData <- readResultsFiles(
  databaseFolders = databaseFolders,
  fileName = "cohort_time_series_data.csv"
)

analysisInfo <- readAnalysisListFiles(databaseFolders)
analysisList <- analysisInfo$analysisList
analyses <- analysisInfo$analyses

cohortCount <- merge(cohortCount, cohorts)
