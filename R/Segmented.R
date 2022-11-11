#' Create a parameter object for the function createSegmentedModel
#'
#' @details
#' Create an object defining the parameter values for use with the 
#' createSegmentedModel. 
#'
#' @param modelType  One of: linear or poisson
#' @param seg.z  the segmented variables(s), i.e. the continuous covariate(s) understood to have a piecewise-linear relationship 
#'               with response. It is a formula with no response variable, such as seg.Z=~x or seg.Z=~x1+x2.
#' @param psi  	 Starting values for the breakpoints to be estimated. If there is a single segmented variable specified in seg.Z, psi is a numeric vector
#' @param npsi  A named vector or list meaning the number (and not locations) of breakpoints to be estimated. 
#' @param fixed.psi  An optional named list meaning the breakpoints to be kept fixed during the estimation procedure. The names should be a subset of (or even the same) variables specified in seg.Z. If there is a single variable in seg.Z, a simple numeric vector can be specified. 
#' @param priorOutcomeLookback  How many days should we look back when identifying prior outcomes?
#' @param control  a list of parameters for controlling the fitting process. See the documentation for [segmented::seg.control] for details
#' 
#' @seealso [segmented::segmented()] which this function wraps its arguments.
#'
#' @export
createSegmentedArgs <- function(modelType,
                                seg.Z=~id,
                                psi,
                                npsi,
                                fixed.psi=NULL, 
                                control= segmented::seg.control(), 
                                model=TRUE) {
  analysis <- list()
  for (name in names(formals(createSegmentedArgs))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- c("SegmentedArgs", "args")
  return(analysis)
}

#' Create a segmented linear model
#'
#' @details
#' Create a segmented linear model using the time-series data
#' and arguments from \code{createSegmentedArgs}.
#'
#' @param tsData  The time series data to use for fitting the model
#' @param segmentedArgs  The segmented linear model arguments. See \code{createSegmentedArgs}.
#'                       for more details.
#'
#' @export
createSegmentedModel <- function(tsData,
                                 segmentedArgs) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = tsData, min.rows = 1, add = errorMessages)
  checkmate::assert_class(x = segmentedArgs, classes = c("SegmentedArgs"), add = errorMessages)
  checkmate::assert_choice(x = segmentedArgs$modelType, choices = c("poisson", "linear"))
  
  if (tolower(segmentedArgs$modelType) == "poisson") {
    obj <- stats::glm(eventCount ~ id, data = data, family = stats::poisson)
  } else {
    obj <- stats::lm(eventCount ~ id, data = data)
  }
  
  segModel <- NULL
  tryCatch(expr = {
    segModel <- segmented::segmented(obj = obj,
                                     seg.Z = segmentedArgs$seg.Z,
                                     psi = segmentedArgs$psi,
                                     npsi = segmentedArgs$npsi,
                                     fixed.psi = segmentedArgs$fixedPsi,
                                     control = segmentedArgs$control,
                                     model = segmentedArgs$model)
  }, error = function(e) {
    ParallelLogger::logError(e)
  }, warning = function(e) {
    ParallelLogger::logWarn(e)
  })
  
  invisible(segModel)
}