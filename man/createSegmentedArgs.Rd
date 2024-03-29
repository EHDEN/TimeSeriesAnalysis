% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Segmented.R
\name{createSegmentedArgs}
\alias{createSegmentedArgs}
\title{Create a parameter object for the function createSegmentedModel}
\usage{
createSegmentedArgs(
  modelType,
  seg.Z = ~eventDate,
  psi = NA,
  npsi = NA,
  fixed.psi = NULL,
  control = segmented::seg.control(),
  model = TRUE
)
}
\arguments{
\item{modelType}{One of: linear or poisson}

\item{seg.Z}{the segmented variables(s), i.e. the continuous covariate(s) understood to have a piecewise-linear relationship
with response. It is a formula with no response variable, such as seg.Z=~x or seg.Z=~x1+x2.}

\item{psi}{Starting values for the breakpoints to be estimated. If there is a single segmented variable specified in seg.Z, psi is a numeric vector}

\item{npsi}{A named vector or list meaning the number (and not locations) of breakpoints to be estimated.}

\item{fixed.psi}{An optional named list meaning the breakpoints to be kept fixed during the estimation procedure. The names should be a subset of (or even the same) variables specified in seg.Z. If there is a single variable in seg.Z, a simple numeric vector can be specified.}

\item{control}{a list of parameters for controlling the fitting process. See the documentation for [segmented::seg.control] for details}

\item{model}{logical value indicating if the model.frame should be returned}
}
\description{
Create a parameter object for the function createSegmentedModel
}
\details{
Create an object defining the parameter values for use with the
createSegmentedModel.
}
\seealso{
[segmented::segmented()] which this function wraps its arguments.
}
