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

getCovariateDataSubset <- function(cohortId, databaseList, comparatorCohortId = NULL) {
  if (usingDbStorage()) {
    return(getCovariateValue(connPool, cohortId = cohortId, databaseList = databaseList, comparatorCohortId = comparatorCohortId))
  } else {
    return(covariateValue[covariateValue$cohortId %in% c(cohortId, comparatorCohortId) & covariateValue$databaseId %in% databaseList, ])
  }
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

plotCpdSegmented <- function(data, model_cpt) {
  df <- data[,c("id", "eventCount", "monthYear")]
  my.fitted <- model_cpt$fitted.values
  my.model <- data.frame(eventCount = my.fitted, id = df$id)
  # get n estimated cpts
  ncpts_est <- nrow(model_cpt$psi)  # one estimated cpt in one row
  # get cpts
  cpts_initial <- model_cpt$psi[1:ncpts_est, 1]  # first column is the initial cpts, one cpt per row
  cpts_segmented <- model_cpt$psi[1:ncpts_est, 2]  # secnd column is the final   cpts, one cpt per row
  confInterval <- segmented::confint.segmented(model_cpt)

  plot <- ggplot2::ggplot(df, ggplot2::aes(x = id, y = eventCount)) +
    ggplot2::geom_point(shape = 16, size = 2, color = "black") +
    ggplot2::scale_x_continuous(breaks = as.vector(df$id), labels = as.vector(df$monthYear)) +
    ggplot2::labs(x = "Time", y = "Drug Exposures (n)") +
    ggplot2::geom_line(data = my.model, color = "blue", ggplot2::aes(x = id, y = eventCount))
  
  # Plot the initial change point specified
  plot <- plot + ggplot2::geom_vline(xintercept=cpts_initial, linetype="longdash", color="gray", size = 1)
  
  # add estimated cpts in red
  for (cp in 1:length(cpts_segmented)) {
    # Add a vertical line and data point for the breakpoint
    bp <- round(cpts_segmented[cp])
    breakpoint <- data.frame(id = bp, eventCount = my.fitted[bp])
    plot <- plot + ggplot2::geom_point(data = breakpoint, color = "red", size = 3, shape=17) +
      #ggplot2::geom_vline(xintercept=bp, linetype="longdash", color="red", size = 1) +
      # Add the confidence interval for the change point
      ggplot2::annotate("pointrange", 
                        x = breakpoint$id, 
                        y = breakpoint$eventCount, 
                        xmin = max(1,confInterval[2]), 
                        xmax = round(confInterval[3]),
                        colour = "red", 
                        linetype="longdash", 
                        size = .5, 
                        alpha = .5)
  }
  plot
  return(plot)
}

shinyServer(function(input, output, session) {
  output$cohortCountsTable <- renderDataTable({
    databaseIds <- unique(cohortCount$databaseIds)
    table <- cohortCount[,c("cohortId", "name", "databaseId", "cohortSubjects", "cohortEntries")]
    names(table) <- c("Cohort ID", "Name", "Database", "Subjects", "Entries")

    # sketch <- htmltools::withTags(table(
    #   class = 'display',
    #   thead(
    #     tr(
    #       th(rowspan = 2, 'Cohort'),
    #       th(rowspan = 2, 'Strata'),
    #       lapply(databaseIds, th, colspan = 1, class = "dt-center")
    #     ),
    #     tr(
    #       lapply(rep(c("Subjects"), length(databaseIds)), th)
    #     )
    #   )
    # ))
    # 
    # sortCallback <- c(
    #   "var dt = table.table().node();",
    #   "$(dt).on('order.dt', function(e, ctx, order) {",
    #   "console.log(order);",
    #   " if (Array.isArray(order) && order.length > 0) {",
    #   "    console.log(order[0]);",
    #   "    col = order[0].col;",
    #   "    if (col < 2) {",
    #   "      var api = new $.fn.DataTable.Api(this);",
    #   "      var orderingArr = [];",
    #   "      for (var i=0 ; i<order.length ; i++) {",
    #   "        orderingArr.push(order[i].col);",
    #   "      }",
    #   #"      api.rowGroup().dataSrc(orderingArr);",
    #   "    }",
    #   "  }",
    #   "})"
    # )
    # columnDefs = list(
    #   #list(targets = c(0), visible = 0),
    #   minCellCountDef(2:(length(databaseIds) + 1))
    # )
    # dtSettings <- getDataTableSettings();
    # dtSettings$options <- append(dtSettings$options, list(columnDefs = columnDefs))
    
    dataTable <- datatable(table,
                           #callback = JS(sortCallback),
                           rownames = FALSE,
                           #container = sketch, 
                           #escape = FALSE,
                           #options = dtSettings$options,
                           #extensions = dtSettings$extensions,
                           class = "stripe nowrap compact")
    return(dataTable)    
  })
  
  output$tsPlot <- renderPlot({
    resultModel <- readRDS("data/linear_t1000_e2000_w1_cdm_optum_ehr_covid_v1547.rds")
    dexTrend <- trendsByMonthYear[trendsByMonthYear$cohortDefinitionId == 1000 & 
                                    trendsByMonthYear$eventCohortDefinitionId == 2000 &
                                    trendsByMonthYear$windowId == 1 & 
                                    trendsByMonthYear$databaseId == 'cdm_optum_ehr_covid_v1547',]
    dexTrend <- dexTrend[order(dexTrend$year, dexTrend$month), ]
    dexTrend <- dexTrend %>% mutate(id = row_number(), 
                                    monthYear = paste(month, year, sep="-"))
    plot <- plotCpdSegmented(dexTrend,
                             model_cpt = resultModel[[1]]$models)
    return(plot)
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
