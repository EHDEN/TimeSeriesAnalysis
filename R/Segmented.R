#' Create a parameter object for the function createSegmentedModel
#'
#' @details
#' Create an object defining the parameter values for use with the
#' createSegmentedModel.
#'
#' @param modelType  One of: linear or poisson
#' @param seg.Z  the segmented variables(s), i.e. the continuous covariate(s) understood to have a piecewise-linear relationship
#'               with response. It is a formula with no response variable, such as seg.Z=~x or seg.Z=~x1+x2.
#' @param psi  	 Starting values for the breakpoints to be estimated. If there is a single segmented variable specified in seg.Z, psi is a numeric vector
#' @param npsi  A named vector or list meaning the number (and not locations) of breakpoints to be estimated.
#' @param fixed.psi  An optional named list meaning the breakpoints to be kept fixed during the estimation procedure. The names should be a subset of (or even the same) variables specified in seg.Z. If there is a single variable in seg.Z, a simple numeric vector can be specified.
#' @param control  a list of parameters for controlling the fitting process. See the documentation for [segmented::seg.control] for details
#' @param model logical value indicating if the model.frame should be returned
#'
#' @seealso [segmented::segmented()] which this function wraps its arguments.
#'
#' @export
createSegmentedArgs <- function(modelType,
                                seg.Z = ~eventDate,
                                psi = NA,
                                npsi = NA,
                                fixed.psi = NULL,
                                control = segmented::seg.control(),
                                model = TRUE) {
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
  checkmate::assert_choice(x = segmentedArgs$modelType, choices = c("poisson", "linear"), add = errorMessages)
  

  # Convert the eventDate to a double for training the model
  tsData <- tsData %>%
    mutate(eventDate = as.double(.data$eventDate))
  
  if (tolower(segmentedArgs$modelType) == "poisson") {
    obj <- stats::glm(eventCount ~ eventDate, data = tsData, family = stats::poisson)
  } else {
    obj <- stats::lm(eventCount ~ eventDate, data = tsData)
  }

  segModel <- NULL
  tryCatch(expr = {
    # The segmented package does not use default values for psi/npsi so
    # adding handling here to call the package in a variety of ways.
    if (is.na(segmentedArgs$psi) && is.na(segmentedArgs$npsi)) {
      segModel <- segmented::segmented(
        obj = obj,
        seg.Z = segmentedArgs$seg.Z,
        fixed.psi = segmentedArgs$fixed.psi,
        control = segmentedArgs$control,
        model = segmentedArgs$model
      )
    } else if (is.na(segmentedArgs$psi) && !is.na(segmentedArgs$npsi)) {
      segModel <- segmented::segmented(
        obj = obj,
        seg.Z = segmentedArgs$seg.Z,
        npsi = segmentedArgs$npsi,
        fixed.psi = segmentedArgs$fixed.psi,
        control = segmentedArgs$control,
        model = segmentedArgs$model
      )
    } else {
      segModel <- segmented::segmented(
        obj = obj,
        seg.Z = segmentedArgs$seg.Z,
        psi = segmentedArgs$psi,
        fixed.psi = segmentedArgs$fixed.psi,
        control = segmentedArgs$control,
        model = segmentedArgs$model
      )
    }
  }, error = function(e) {
    message(paste0("Error encountered when fitting segmented model: ", e$message))
  }, warning = function(e) {
    message(paste0("Warning encountered when fitting segmented model: ", e$message))
  })

  invisible(list(tsData = tsData, model = segModel))
}
