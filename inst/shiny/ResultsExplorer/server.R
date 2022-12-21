library(shiny)
library(shinydashboard)
library(DT)
library(htmltools)
library(ggplot2)
library(dplyr)

stackedBarChart <- function(table, rows, cols, title, show.legend=F, legend.position="top") {
  # Note: drugLevels is a global variable
  table <- table %>%
    arrange(
      #factor(databaseDescription, levels = rev(databaseList$databaseDescription)),
      factor(group, levels=rev(drugLevels))
    ) %>%
    mutate(
      #databaseDescription=factor(databaseDescription, levels=rev(databaseList$databaseDescription)),
      group=factor(group,levels=rev(drugLevels))
    )

  p <- ggplot(table, aes(fill=group, y=percentage, x=year)) + 
    geom_bar(position="fill", stat="identity", show.legend = show.legend) +
    drugLevelsColorBrew +
    labs(x = "Year", y = "Percentage (%)", title = title) +
    scale_y_continuous(labels=scales::percent) +
    guides(fill=guide_legend(title="Treatment"))
  
  if (show.legend) {
    p <- p + theme(legend.position = legend.position)
  }

  p <- p + facet_wrap(facets = vars(table$databaseDescription),
                      nrow=rows,
                      ncol=cols)
  return(p)
}

truncateStringDef <- function(columns, maxChars) {
  list(
    targets = columns,
    render = JS(sprintf("function(data, type, row, meta) {\n
      return type === 'display' && data != null && data.length > %s ?\n
        '<span title=\"' + data + '\">' + data.substr(0, %s) + '...</span>' : data;\n
     }", maxChars, maxChars))
  )
}

minCellCountDef <- function(columns) {
  list(
    targets = columns,
    render = JS("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return data.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
    return '<' + Math.abs(data).toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
  }")
  )
}

minCellPercentDef <- function(columns) {
  list(
    targets = columns,
    render = JS("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return (100 * data).toFixed(1).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,') + '%';
    return '<' + Math.abs(100 * data).toFixed(1).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,') + '%';
  }")
  )
}

minCellRealDef <- function(columns, digits = 1) {
  list(
    targets = columns,
    render = JS(sprintf("function(data, type) {
    if (type !== 'display' || isNaN(parseFloat(data))) return data;
    if (data >= 0) return data.toFixed(%s).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
    return '<' + Math.abs(data).toFixed(%s).replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,');
  }", digits, digits))
  )
}

styleAbsColorBar <- function(maxValue, colorPositive, colorNegative, angle = 90) {
  JS(sprintf("isNaN(parseFloat(value))? '' : 'linear-gradient(%fdeg, transparent ' + (%f - Math.abs(value))/%f * 100 + '%%, ' + (value > 0 ? '%s ' : '%s ') + (%f - Math.abs(value))/%f * 100 + '%%)'", 
             angle, maxValue, maxValue, colorPositive, colorNegative, maxValue, maxValue))
}

getDataTableSettings <- function() {
  dtSettings <- list(
    options = list(pageLength = 25,
                   lengthMenu = c(25, 50, 100, -1),
                   searching = TRUE,
                   lengthChange = TRUE,
                   ordering = TRUE,
                   paging = TRUE,
                   info = TRUE,
                   scrollX = TRUE),
    extensions = list() #list('Buttons') #'RowGroup'
  )
                     
  return(dtSettings)
}

renderBorderTag <-  function() {
  return(htmltools::withTags(
    div(class="cohort-heading")
  ))
}

ggplotCpd <- function(trendData, segmentedModel, xAxisLabel = "Time", yAxisLabel = "Event Count (n)") {
  # get n estimated cpts
  ncpts_est <- segmentedModel$ncpts
  # get cpts
  cptsInitial <- segmentedModel$model$psi[1:ncpts_est, 1]  # first column is the initial cpts, one cpt per row
  cptsSegmented <- segmentedModel$model$psi[1:ncpts_est, 2]  # secnd column is the final   cpts, one cpt per row
  confInterval <- segmentedModel$confint
  my.fitted <- stats::fitted(segmentedModel$model)
  my.model <- data.frame(eventCount = my.fitted, id = trendData$id)
  
  plot <- ggplot2::ggplot(trendData, ggplot2::aes(x = id, y = eventCount)) +
    ggplot2::geom_point(shape = 16, size = 2, color = "black") +
    ggplot2::scale_x_continuous(breaks = as.vector(trendData$id), labels = as.vector(trendData$yearMonth)) +
    ggplot2::labs(x = xAxisLabel, y = yAxisLabel) +
    ggplot2::geom_line(data = my.model, color = "blue", ggplot2::aes(x = id, y = eventCount))
  
  # Plot the initial change point specified
  plot <- plot + ggplot2::geom_vline(xintercept=cptsInitial, linetype="longdash", color="gray", size = 1)
  
  # add estimated cpts in red
  for (cp in 1:length(cptsSegmented)) {
    # Add a vertical line and data point for the breakpoint
    bp <- round(cptsSegmented[cp])
    breakpoint <- data.frame(id = bp, eventCount = my.fitted[bp])
    plot <- plot + ggplot2::geom_point(data = breakpoint, color = "red", size = 3, shape=17) +
      #ggplot2::geom_vline(xintercept=bp, linetype="longdash", color="red", size = 1) +
      # Add the confidence interval for the change point
      #ggplot2::geom_pointrange(orientation="x",ggplot2::aes(ymin=confInterval[2],ymax=confInterval[3],color="red"))
      #ggplot2::geom_hline(ggplot2::aes(xmin=confInterval[2],xmax=confInterval[3],color="red"))
      if (!is.null(confInterval)) {
        ggplot2::annotate("pointrange", x = breakpoint$id, y = breakpoint$eventCount, xmin = max(1,confInterval[2]), xmax = round(confInterval[3]),
                          colour = "red", linetype="longdash", size = .5, alpha = .5)
      }
  }  
  
  return(plot)
}

getModelFileName <- function(analysisId, dataSetId) {
  fileName <- sprintf("Analysis%d/ts_d%d.rds", analysisId, dataSetId)
  invisible(fileName)
}


shinyServer(function(input, output, session) {
  analysisId <- reactive({
    return(analyses[analyses$description == input$tsAnalysis,]$analysisId[1])
  })
  
  dataSetId <- reactive({
    return(cohorts[cohorts$cohortName == input$targetCohort,]$cohortId[1])
  })
  
  output$cohortCountsTable <- renderDataTable({
    databaseIds <- unique(cohortCount$databaseId)
    table <- cohortCount[,c("cohortId", "cohortName", "databaseId", "cohortSubjects", "cohortEntries")]
    names(table) <- c("Cohort ID", "Name", "Database", "Subjects", "Entries")
    columnDefs = list(
      minCellCountDef(c(3,4))
    )
    dtSettings <- getDataTableSettings()
    dtSettings$options <- append(dtSettings$options, list(columnDefs = columnDefs))
    
    dataTable <- datatable(table,
                           rownames = FALSE,
                           escape = FALSE,
                           options = dtSettings$options,
                           extensions = dtSettings$extensions,
                           class = "stripe nowrap compact")
    return(dataTable)    
  })
  
  output$tsPlot <- renderPlot({
    resultModelFileName <- getModelFileName(analysisId = analysisId(),
                                            dataSetId = dataSetId())
    #print(resultModelFileName)
    analysisOutput <- readRDS(file.path(dataFolder, resultModelFileName))
    
    if ("SegmentedArgs" %in% class(analysisList[[analysisId()]]$tsArgs)) {
      plot <- TimeSeriesAnalysis::plotSegmented(tsData = analysisOutput$tsData,
                                                model = analysisOutput$model,
                                                plotSubtitle = input$databases)
    } else if ("OcpArgs" %in% class(analysisList[[analysisId()]]$tsArgs)) {
      plot <- TimeSeriesAnalysis::plotOcp(tsData = analysisOutput$tsData,
                                          model = analysisOutput$model,
                                          plotSubtitle = input$databases)
    } else {
      stop(paste0("An unknown time series model found: ", class(analysisList[[analysisId()]]$tsArgs), ". Stopping the execution."))
    }

    return(plot)
    # plot(analysisOutput$model, conf.level=0.95, shade=TRUE, type = "l", xlab = "Time", ylab = "N Drugs", main = paste("Segmented Linear Regression, npsi = 1"))# plots regression lines of the two segments using the coeffs returned in o.seg
    # points(analysisOutput$tsData$eventDate,analysisOutput$tsData$eventCount, xlab = "Event Date", ylab = "Event Count", cex= 1.5, pch=16)# add the actual time series, ,type = "l"
    
  }, res = 100)
  
  # Database Info ------------------
  output$borderDatabaseInformation <- renderUI({
    return(renderBorderTag())
  })
  
  output$databaseInformationTable <- renderDataTable({

    table <- database #database[, c("databaseId", "databaseName", "description", "termsOfUse")]
    options = list(pageLength = 25,
                   searching = TRUE,
                   lengthChange = FALSE,
                   ordering = TRUE,
                   paging = FALSE#,
                   #columnDefs = list(list(width = '10%', targets = 0),
                    #                 list(width = '20%', targets = 1),
                    #                 list(width = '35%', targets = 2))
    )
    table <- datatable(table,
                       options = options,
                       #colnames = c("ID", "Name", "Description", "Terms of Use"),
                       rownames = FALSE,
                       class = "stripe compact")
    return(table)
  })
  
  output$dlDatabaseInformation <- downloadHandler(
    filename = function() {
      "database_info.csv"
    },
    content = function(file) {
      table <- database[, c("databaseId", "databaseName", "description")]
      write.csv(table, file, row.names = FALSE, na = "")
    }
  )
})
