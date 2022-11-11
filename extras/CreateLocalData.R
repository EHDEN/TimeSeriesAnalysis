trendsData <- CohortGenerator::readCsv(file = "D:/TimeSeriesAnalysis/Run/cdm_optum_ehr_v1892/trends_by_month_year.csv")
trendsData <- trendsData[,c("cohortDefinitionId", "month", "year", "eventCount", "databaseId")]

dusData <- CohortGenerator::readCsv(file = "D:/git/ehden/TimeSeriesAnalysis/data/COVID-DUS-Hydrox-Dex.csv")

dusDataSubsetHCQ <- dusData[dusData$DATABASE == 'IQVIA-OpenClaims' & dusData$group == 'Hydroxychloroquine',]
dusDataSubsetHCQ$cohortDefinitionId <- 1
dusDataSubsetHCQ <- dusDataSubsetHCQ[,c("cohortDefinitionId", "group", "MONTH", "YEAR", "n", "DATABASE")]
colnames(dusDataSubsetHCQ) <- c("cohortDefinitionId", "cohortName", "month","year","eventCount","databaseId" )

dusDataSubsetDEX <- dusData[dusData$DATABASE == 'IQVIA-OpenClaims' & dusData$group == 'Dexamethasone',]
dusDataSubsetDEX$cohortDefinitionId <- 2
dusDataSubsetDEX <- dusDataSubsetDEX[,c("cohortDefinitionId", "group", "MONTH", "YEAR", "n", "DATABASE")]
colnames(dusDataSubsetDEX) <- c("cohortDefinitionId", "cohortName", "month","year","eventCount","databaseId" )

drugData <- rbind(dusDataSubsetHCQ,dusDataSubsetDEX)
drugData$databaseId <- "example_database"

usethis::use_data(drugData)