library(shiny)
library(shinydashboard)
library(DT)

ohdsiBlueHex <- "#20425A"
ohdsiLightYellowHex <- "FBC209"

addInfo <- function(item, infoId, class = NULL, style = NULL) {
  if (is.null(class)) {
    class = "badge pull-right action-button"
  }
  if (is.null(style)) {
    style = "padding: 1px 6px 2px 6px; background-color: steelblue;"
  }
  infoTag <- tags$small(class = class,
                        style = style,
                        type = "button", 
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}

exploreOptions <- box(
  conditionalPanel(condition = "input.tabs=='timeSeries'",
                   shinyWidgets::pickerInput("databases", "Database",
                                             choices = database$databaseId,
                                             selected = database$databaseId,
                                             options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                             multiple = FALSE)
  ),
  conditionalPanel(condition = "input.tabs=='timeSeries'",
                   shinyWidgets::pickerInput("tsAnalysis", "Analysis",
                                             choices = analyses$description,
                                             selected = analyses$description[1],
                                             options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                             multiple = FALSE),
                   shinyWidgets::pickerInput("targetCohort", "Target Cohort",
                                             choices = cohorts$cohortName,
                                             selected = cohorts$cohortName[1],
                                             options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                                             multiple = FALSE),
                   # shinyWidgets::pickerInput("eventCohort", "Event",
                   #                           choices = eventCohorts,
                   #                           selected = eventCohorts,
                   #                           options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE, dropupAuto = FALSE),
                   #                           multiple = FALSE)
  ),
  # conditionalPanel(condition = "input.tabs=='timeSeries'",
  #                  shinyWidgets::pickerInput("timeWindow", "Time Window",
  #                                            choices = eventTimeWindows,
  #                                            selected = eventTimeWindows,
  #                                            options = shinyWidgets::pickerOptions(actionsBox = TRUE, dropupAuto = FALSE),
  #                                            multiple = FALSE)
  # ),
  # conditionalPanel(condition = "input.tabs=='timeSeries'",
  #                  shinyWidgets::pickerInput("modelType", "Model",
  #                                            choices = modelTypes,
  #                                            selected = modelTypes[1],
  #                                            options = shinyWidgets::pickerOptions(actionsBox = TRUE, dropupAuto = FALSE),
  #                                            multiple = FALSE)
  # ),
  width=12, 
  title="Explore results by:", 
  collapsible = TRUE
)

dashboardPage(
  dashboardHeader(
    title = "Time Series Analysis",
    tags$li(div(img(src = 'logo.png',
                    title = "EHDEN Time Series Analysis", 
                    height = "40px", 
                    width = "40px"),
                style = "padding-top:0px; padding-bottom:0px;"),
            class = "dropdown")    
    ),  
  dashboardSidebar(
    tags$head(tags$style(HTML(paste0('.main-header { background-color: ', ohdsiBlueHex, '; }')))),
    sidebarMenu(id = "tabs",
                menuItem("About", tabName = "about"),
                menuItem("Cohort Counts", tabName = "counts"),
                menuItem("Time Series", tabName = "timeSeries"),
                menuItem("Database information", tabName = "databaseInformation")#,
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="ohdsi.css")
    ),
    tags$div(
      conditionalPanel(condition = "input.tabs=='timeSeries'",
                       exploreOptions
      )
    ),
    ### changing theme
    tabItems(
      tabItem(tabName = "about",
        includeMarkdown("md/about.md")
      ),
      tabItem(tabName = "counts",
        dataTableOutput("cohortCountsTable")
      ),
      tabItem(tabName = "timeSeries",
        plotOutput("tsPlot")
      ),
     tabItem(tabName = "databaseInformation",
              downloadButton("dlDatabaseInformation", "Download Data"),
              htmlOutput("borderDatabaseInformation"),
              dataTableOutput("databaseInformationTable")
      )
    )
  )
)
