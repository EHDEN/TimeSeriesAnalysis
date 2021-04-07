createAndGetTrendsByTargetEvent <- function(connection,
                                            cohortDatabaseSchema,
                                            cohortTable,
                                            tempEmulationSchema) {
  # Create the trends summary
  packageName <- getThisPackageName()
  targetEventXref <- getTargetEventXref()
  eventTimeWindows <- getEventTimeWindows()
  eventTimeWindowTempTableSql <- getEventWindowsTempTableSql(connection, eventTimeWindows, tempEmulationSchema)
  targetEventXrefTempTableSql <- getTargetEventXrefTempTableSql(connection, targetEventXref, tempEmulationSchema)
  sql <- SqlRender::loadRenderTranslateSql(dbms = attr(connection, "dbms"),
                                           sqlFilename = "CreateTrendsByTargetEvent.sql",
                                           packageName = packageName,
                                           tempEmulationSchema = tempEmulationSchema,
                                           warnOnMissingParameters = TRUE,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable,
                                           event_time_window_table_create = eventTimeWindowTempTableSql$create,
                                           event_time_window_table_drop = eventTimeWindowTempTableSql$drop,
                                           target_event_xref_table_create = targetEventXrefTempTableSql$create,
                                           target_event_xref_table_drop = targetEventXrefTempTableSql$drop)
  
  ParallelLogger::logInfo("Compute trends for all target and event cohorts")
  DatabaseConnector::executeSql(connection, sql)
  
  # Retrieving the trends summary
  sql <- "SELECT * FROM #trends ORDER BY cohort_definition_id, event_cohort_definition_id, window_id, year, month"
  sql <- SqlRender::translate(sql = sql, targetDialect = attr(connection, "dbms"), tempEmulationSchema = tempEmulationSchema)
  ParallelLogger::logInfo("Retrieving results")
  results <- DatabaseConnector::querySql(connection, sql)
  names(results) <- SqlRender::snakeCaseToCamelCase(names(results))
  return(results)
}

getTargetEventXrefTempTableSql <- function(connection, targetEventXref, tempEmulationSchema) {
  sql <- "CREATE TABLE #target_event_xref (
            target_cohort_id bigint NOT NULL,
            event_cohort_id bigint NOT NULL
          )
          ;
          
          INSERT INTO #target_event_xref (target_cohort_id, event_cohort_id)
          SELECT a.target_cohort_id, a.event_cohort_id
          FROM (
            @unions
          ) as a;"
  unions <- "";
  for(i in 1:nrow(targetEventXref)) {
    stmt <- paste0("SELECT ", targetEventXref$targetCohortId[i], " target_cohort_id, ", 
                   targetEventXref$eventCohortId[i], " event_cohort_id")
    unions <- paste(unions, stmt, sep="\n")
    if (i < nrow(targetEventXref)) {
      unions <- paste(unions, "UNION ALL", sep="\n")
    }
  }
  
  sql <- SqlRender::render(sql, unions = unions)
  sql <- SqlRender::translate(sql = sql, 
                              targetDialect = attr(connection, "dbms"),
                              tempEmulationSchema = tempEmulationSchema)
  
  dropSql <- "TRUNCATE TABLE #target_event_xref;\nDROP TABLE #target_event_xref;\n\n"
  dropSql <- SqlRender::translate(sql = dropSql, 
                                  targetDialect = attr(connection, "dbms"),
                                  tempEmulationSchema = tempEmulationSchema)
  return(list(create = sql, drop = dropSql))
}

getEventWindowsTempTableSql <- function(connection, eventWindows, tempEmulationSchema) {
  sql <- "CREATE TABLE #event_windows (
            window_id int NOT NULL,
            window_start int NOT NULL,
            window_end int NOT NULL,
            window_type varchar(100) NOT NULL
          )
          ;
          
          INSERT INTO #event_windows (window_id,window_start,window_end,window_type)
          SELECT a.window_id,a.window_start,a.window_end,a.window_type
          FROM (
            @unions
          ) as a;"
  unions <- "";
  for(i in 1:nrow(eventWindows)) {
    stmt <- paste0("SELECT ", eventWindows$windowId[i], " window_id, ", 
                   eventWindows$windowStart[i], " window_start, ", 
                   eventWindows$windowEnd[i], " window_end, ", 
                   "'", eventWindows$windowType[i], "' window_type")
    unions <- paste(unions, stmt, sep="\n")
    if (i < nrow(eventWindows)) {
      unions <- paste(unions, "UNION ALL", sep="\n")
    }
  }
  
  sql <- SqlRender::render(sql, unions = unions)
  sql <- SqlRender::translate(sql = sql, 
                              targetDialect = attr(connection, "dbms"),
                              tempEmulationSchema = tempEmulationSchema)
  
  dropSql <- "TRUNCATE TABLE #event_windows;\nDROP TABLE #event_windows;\n\n"
  dropSql <- SqlRender::translate(sql = dropSql, 
                                  targetDialect = attr(connection, "dbms"),
                                  tempEmulationSchema = tempEmulationSchema)
  return(list(create = sql, drop = dropSql))
}

getTargetEventXref <- function() {
  targetEventXref <- readCsv("settings/targetEventXref.csv")
  return(targetEventXref)  
}

getEventTimeWindows <- function() {
  eventTimeWindows <- readCsv("settings/eventTimeWindows.csv")
  return(eventTimeWindows)
}
