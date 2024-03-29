% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot.R
\name{plotOcp}
\alias{plotOcp}
\title{Plots time series data with change points computed by the ocp package}
\usage{
plotOcp(
  tsData,
  model,
  plotTitle = "Bayesian Online Changepoint Detection",
  plotSubtitle = ""
)
}
\arguments{
\item{tsData}{The time series data used to train the model}

\item{model}{The trained ocp model}

\item{plotTitle}{The title for the plot}

\item{plotSubtitle}{The subtitle for the plot}
}
\description{
Plots time series data with change points computed by the ocp package
}
\details{
Create a plot to display the time series data along with any change points
detected by the ocp package
}
