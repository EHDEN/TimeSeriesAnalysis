#' Create a parameter object for the function createOcpModel
#'
#' @details
#' Create an object defining the parameter values for use with the 
#' createOcpModel function 
#'
#' @param oCPD   ocp object computed in a previous run of an algorithm. it can be built upon with the input data points, as long as the settings for both are the same.
#' @param missPts	This setting indicates how to deal with missing points (e.g. NA). The options are: "mean", "prev", "none", and a numeric value. If the data is multivariate. The numeric replacement value could either be a single value which would apply to all dimensions, or a vector of the same length as the number of dimensions of the data.
#' @param hazardFunc This setting allows choosing a hazard function, and also setting the constants within that function. For example, the default hazard function is: function(x, lambda)const_hazard(x, lambda=100) and the lambda can be set as appropriate.
#' @param probModel This parameter is a function to be used to calculate the predictive probabilities and update the parameters of the model. The default setting uses a gaussian underlying distribution: "gaussian"
#' @param initParams The parameters used to initialize the probability model. The default settings correspond to the input default gaussian model.
#' @param multivariate This setting indicates if the incoming data is multivariate or univariate.
#' @param cpthreshold Probability threshold for the method of extracting a list of all changepoints that have a run length probability higher than a specified value. The default is set to 0.5.
#' @param truncRlim 	The probability threshold to begin truncating the R vector. The R vector is a vector of run-length probabilities. To prevent truncation, set this to 0. The defaults setting is 10^(-4) as suggested by the paper.
#' @param minRlength	The minimum size the run length probabilities vector must be before beginning to check for the truncation threshold.
#' @param maxRlength	The maximum size the R vector is allowed to be, before enforcing truncation to happen.
#' @param minsep	  This setting constrains the possible changepoint locations considered in determining the optimal set of changepoints. It prevents considered changepoints that are closer together than the value of minsep. The default is 3.
#' @param maxsep This setting constrains the possible changepoint locations considered in determining the optimal set of changepoints. It prevents considered changepoints that are closer farther apart than the value of maxsep. The default is 100.
#' @param timing To print out times during the algorithm running, to track its progress, set this setting to true.
#' @param getR	To output the full R matrix, set this setting to TRUE. Outputting this matrix causes a major slow down in efficiency.
#' @param optionalOutputs	Output additional values calculated during running the algorithm, including a matrix containing all the input data, the predictive probability vectors at each step of the algorithm, and the vector of means at each step of the algorithm.
#' @param printupdates	This setting prints out updates on the progress of the algorithm if set to TRUE.
#' 
#' 
#' @seealso [ocp::onlineCPD()] which this function wraps its arguments.
#'
#' @export
createOcpArgs <- function(oCPD = NULL,
                          missPts = "none",
                          hazardFunc = function(x, lambda) {ocp::const_hazard(x, lambda = 100)},
                          probModel = list("g"),
                          initParams = list(list(m = 0, k = 0.01, a = 0.01, b = 1e-04)),
                          multivariate = FALSE, 
                          cpthreshold = 0.5,
                          truncRlim = .Machine$double.xmin, 
                          minRlength = 1,
                          maxRlength = 10^4, 
                          minsep = 1, 
                          maxsep = 10^4, 
                          timing = FALSE,
                          getR = FALSE, 
                          optionalOutputs = FALSE, 
                          printupdates = FALSE
                        ) {
  analysis <- list()
  for (name in names(formals(createOcpArgs))) {
    analysis[[name]] <- get(name)
  }
  class(analysis) <- c("OcpArgs", "args")
  return(analysis)
}

#' Create a Bayesian Online Changepoint model
#'
#' @details
#' Create a Bayesian Online Changepoint model using the time-series data
#' and arguments from \code{createSegmentedArgs}.
#'
#' @param tsData  The time series data to use for fitting the model
#' @param ocpArgs  The OCP linear model arguments. See \code{createSegmentedArgs}.
#'                       for more details.
#'
#' @export
createOcpModel <- function(tsData,
                           ocpArgs) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = tsData, min.rows = 1, add = errorMessages)
  checkmate::assert_class(x = ocpArgs, classes = c("OcpArgs"), add = errorMessages)
  checkmate::assert_choice(x = ocpArgs$missPts, choices = c("mean", "prev", "none"))
  
  ocpModel <- NULL
  tryCatch(expr = {
    ocpModel <- ocp::onlineCPD(datapts = tsData,
                               oCPD = ocpArgs$oCPD,
                               missPts = ocpArgs$missPts,
                               hazard_func = ocpArgs$hazardFunc,
                               probModel = ocpArgs$probModel,
                               init_params = ocpArgs$initParams,
                               multivariate = ocpArgs$multivariate,
                               cpthreshold = ocpArgs$cpthreshold,
                               truncRlim = ocpArgs$truncRlim,
                               minRlength = ocpArgs$minRlength,
                               minsep = ocpArgs$minsep,
                               maxsep = ocpArgs$maxsep,
                               timing = ocpArgs$timing,
                               getR = ocpArgs$getR,
                               optionalOutputs = ocpArgs$optionalOutputs,
                               printupdates = ocpArgs$printupdates)
  }, error = function(e) {
    warning(e)
  }, warning = function(e) {
    warning(e)
  })
  
  invisible(ocpModel)
}