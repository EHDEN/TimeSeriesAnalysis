#' @export
readCsv <- function(resourceFile, packageName = getThisPackageName()) {
  pathToCsv <- system.file(resourceFile, package = packageName, mustWork = TRUE)
  fileContents <- readr::read_csv(pathToCsv, col_types = readr::cols())
  return(fileContents)
}

getTrendsFileName <- function() {
  return("trends_by_month_year.csv")
}

getThisPackageName <- function() {
  return("TimeSeriesAnalysis")
}

getVocabularyInfo <- function(connection, cdmDatabaseSchema, tempEmulationSchema) {
  sql <- "SELECT vocabulary_version FROM @cdm_database_schema.vocabulary WHERE vocabulary_id = 'None';"
  sql <- SqlRender::render(sql, cdm_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"), tempEmulationSchema = tempEmulationSchema)
  vocabInfo <- DatabaseConnector::querySql(connection, sql)
  return(vocabInfo[[1]])
}

getPersonCount <- function(connection, cdmDatabaseSchema, tempEmulationSchema) {
  sql <- "SELECT COUNT_BIG(person_id) FROM @cdm_database_schema.person;"
  sql <- SqlRender::render(sql, cdm_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"), tempEmulationSchema = tempEmulationSchema)
  personCount <- DatabaseConnector::querySql(connection, sql)
  return(personCount[[1]])
}

getObservationPeriodDateRange <- function(connection, cdmDatabaseSchema, tempEmulationSchema) {
  sql <- "SELECT MIN(observation_period_start_date) min_observation_period_date, MAX(observation_period_end_date) max_observation_period_date FROM @cdm_database_schema.observation_period;"
  sql <- SqlRender::render(sql, cdm_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"), tempEmulationSchema = tempEmulationSchema)
  op <- DatabaseConnector::querySql(connection, sql)
  names(op) <- SqlRender::snakeCaseToCamelCase(names(op))
  return(op)
}


enforceMinCellValue <- function(data, fieldName, minValues, silent = FALSE) {
  toCensor <- !is.na(data[, fieldName]) & data[, fieldName] < minValues & data[, fieldName] != 0
  if (!silent) {
    percent <- round(100 * sum(toCensor)/nrow(data), 1)
    ParallelLogger::logInfo("   censoring ",
                            sum(toCensor),
                            " values (",
                            percent,
                            "%) from ",
                            fieldName,
                            " because value below minimum")
  }
  if (length(minValues) == 1) {
    data[toCensor, fieldName] <- -minValues
  } else {
    data[toCensor, fieldName] <- -minValues[toCensor]
  }
  return(data)
}

writeToCsv <- function(data, fileName, incremental = FALSE, ...) {
  colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  if (incremental) {
    params <- list(...)
    names(params) <- SqlRender::camelCaseToSnakeCase(names(params))
    params$data = data
    params$fileName = fileName
    do.call(CohortGenerator::saveIncremental, params)
  } else {
    readr::write_csv(data, fileName)
  }
}


.systemInfo <- function() {
  si <- sessionInfo()
  lines <- c()
  lines <- c(lines, "R version:")
  lines <- c(lines, si$R.version$version.string)
  lines <- c(lines, "")
  lines <- c(lines, "Platform:")
  lines <- c(lines, si$R.version$platform)
  lines <- c(lines, "")
  lines <- c(lines, "Locale:")
  lines <- c(lines, si$locale)
  lines <- c(lines, "")
  lines <- c(lines, "Attached base packages:")
  lines <- c(lines, paste("-", si$basePkgs))
  lines <- c(lines, "")
  lines <- c(lines, "Other attached packages:")
  for (pkg in si$otherPkgs) lines <- c(lines,
                                       paste("- ", pkg$Package, " (", pkg$Version, ")", sep = ""))
  return(paste(lines, collapse = "\n"))
}