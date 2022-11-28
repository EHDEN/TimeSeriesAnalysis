#' Sample drug data for using the change detection time series methods
#'
#' A dataset containing the use of Hydroxychloroquine and Dexamethasone
#' in COVID patients from a large US Claims dataset.
#'
#' @format A data frame with 20 rows and 6 variables:
#' \describe{
#'   \item{cohortDefinitionId}{The cohort definition id}
#'   \item{cohortName}{The drug cohort of interest}
#'   \item{cohortStartDate}{The cohort start date}
#'   \item{timeInterval}{The time interval of capture for the time series data}
#'   \item{subjectCount}{The number of people in the cohort for the month combination}
#'   \item{eventCount}{The number of events in the cohort for the month combination. For cohorts, people may have > 1 event}
#'   \item{databaseId}{The database ID that contributed the data}
#' }
#' @usage data(drugData)
"drugData"
