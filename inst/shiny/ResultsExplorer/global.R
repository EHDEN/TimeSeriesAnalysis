library(dplyr)
library(shiny)
library(DatabaseConnector)

# Borrowed from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
is_installed <- function(pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

if (!exists("shinySettings")) {
  if (file.exists("data")) {
    shinySettings <- list(storage = "filesystem", dataFolder = "data", dataFile="PreMerged.RData")
  } else {
    stop("Results data not found")
  }
}
dataStorage <- shinySettings$storage
dataFolder <- shinySettings$dataFolder
dataFile <- shinySettings$dataFile

#Load the data
load(file.path(dataFolder, dataFile))

readSettingsFile <- function(fileName) {
  contents <- readr::read_csv(file.path("settings", fileName), col_types = readr::cols())
  return(contents)
}

#Load the settings
cohorts <- readSettingsFile("CohortsToCreate.csv")
eventTimeWindowsFromFile <- readSettingsFile("eventTimeWindows.csv")
targetEventXref <- readSettingsFile("targetEventXref.csv")
modelTypes <- c("Linear", "Poisson")

# Format the results
cohortCount <- merge(cohortCount, cohorts)

targetEventXref <- merge(targetEventXref, cohorts[,c("cohortId", "name")], by.x = "targetCohortId", "cohortId")
names(targetEventXref) <- c("targetCohortId", "eventCohortId", "targetCohortName")
targetEventXref <- merge(targetEventXref, cohorts[,c("cohortId", "name")], by.x = "eventCohortId", "cohortId")
names(targetEventXref) <- c("eventCohortId", "targetCohortId","targetCohortName", "eventCohortName")

eventTimeWindowsFromFile$timeDisplayName <- paste0(eventTimeWindowsFromFile$windowStart, "d - ", eventTimeWindowsFromFile$windowEnd, "d")
eventTimeWindowsSubset <- unique(eventTimeWindowsFromFile[,c("windowId", "timeDisplayName")])
eventTimeWindows <- eventTimeWindowsSubset$windowId
names(eventTimeWindows) <- eventTimeWindowsSubset$timeDisplayName

targetCohortsSubset <- unique(targetEventXref[,c("targetCohortId", "targetCohortName")])
targetCohorts <- targetCohortsSubset$targetCohortId
names(targetCohorts) <- targetCohortsSubset$targetCohortName

eventCohortsSubset <- unique(targetEventXref[,c("eventCohortId", "eventCohortName")])
eventCohorts <- eventCohortsSubset$eventCohortId
names(eventCohorts) <- eventCohortsSubset$eventCohortName





