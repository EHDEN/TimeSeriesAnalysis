#' Sample drug data for using the change detection time series methods
#'
#' A dataset containing the use of Hydroxychloroquine and Dexamethasone
#' in COVID patients from a large US Claims dataset.
#'
#' @format A data frame with 20 rows and 6 variables:
#' \describe{
#'   \item{cohortDefinitionId}{The cohort definition id}
#'   \item{cohortName}{The drug cohort of interest}
#'   \item{month}{The month of the cohort}
#'   \item{year}{The year of the cohort}
#'   \item{eventCount}{The number of people in the cohort for the month/year combination}
#'   \item{databaseId}{The database ID that contributed the data}
#' }
"drugData"