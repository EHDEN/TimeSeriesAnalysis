#' Create a parameter object for the function getCohortTimeSeriesData
#'
#' @details
#' Create an object defining the parameter values for use with the
#' @seealso [getCohortTimeSeriesData]
#'
#' @param cohortIds  A vector of cohort Ids; set to -1 to use all cohorts in the cohort table
#' @param timeInterval  The time interval (year, month, day) to use when extracting the cohort time series.
#'
#' @export
createCohortTimeSeriesArgs <- function(cohortIds = -1,
                                       timeInterval = "month") {
  analysis <- list()
  for (name in names(formals(createCohortTimeSeriesArgs))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- c("cohortTimeSeriesArgs", "args")
  return(analysis)
}

#' Retrieve time series data for one or more cohorts
#'
#' @details
#' This function will retrieve a data frame representing
#' entry events for one or more cohorts grouped into time intervals
#' as specified by the cohortTimeSeriesArgs
#'
#' @template Connection
#'
#' @template CdmDatabaseSchema
#'
#' @template TempEmulationSchema
#'
#' @template CohortDatabaseSchema
#'
#' @template CohortTable
#'
#' @param outputFolder The location where the cohort time series data results will be written.
#'
#' @param databaseId The database identifier for the time series data
#'
#' @param cohortTimeSeriesArgs The cohort time series arguments. @seealso[createCohortTimeSeriesArgs] for more information.
#'
#' @export
getCohortTimeSeriesData <- function(connectionDetails = NULL,
                                    connection = NULL,
                                    cdmDatabaseSchema,
                                    tempEmulationSchema = NULL,
                                    cohortDatabaseSchema,
                                    cohortTable = "cohort",
                                    outputFolder,
                                    databaseId,
                                    cohortTimeSeriesArgs) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assert_class(x = cohortTimeSeriesArgs, classes = c("cohortTimeSeriesArgs"), add = errorMessages)
  checkmate::assert_choice(x = cohortTimeSeriesArgs$timeInterval, choices = c("day", "month", "year"))
  start <- Sys.time()

  # Startup Checks -----
  if (!file.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  # Extract the time series information  -----------------------------------------------------
  message("Get time series data")
  tsData <- executeCohortTimeSeriesDataQuery(
    connection = connection,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cohortTimeSeriesArgs = cohortTimeSeriesArgs
  )
  if (nrow(tsData) <= 0) {
    warning("No cohort time series data found. Did you generate the cohorts?")
  } else {
    tsData$databaseId <- databaseId
  }
  CohortGenerator::writeCsv(
    x = tsData,
    file = file.path(outputFolder, "cohort_time_series_data.csv")
  )

  delta <- Sys.time() - start
  rlang::inform(paste("Time series data extract took", signif(delta, 3), attr(delta, "units")))
  invisible(tsData)
}

executeCohortTimeSeriesDataQuery <- function(connection,
                                             cohortDatabaseSchema,
                                             cohortTable = "cohort",
                                             cohortTimeSeriesArgs) {
  sql <- SqlRender::readSql(
    system.file(
      "sql/sql_server/CohortTimeSeries.sql",
      package = "TimeSeriesAnalysis",
      mustWork = TRUE
    )
  )
  sql <- SqlRender::render(
    sql = sql,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    cohort_definition_id = cohortTimeSeriesArgs$cohortIds,
    time_interval = cohortTimeSeriesArgs$timeInterval,
    warnOnMissingParameters = TRUE
  )
  sql <- SqlRender::translate(
    sql = sql,
    targetDialect = connection@dbms
  )
  trends <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  return(trends)
}
