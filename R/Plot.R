#' Plots time series data with change points computed with a segmented generalized
#' linear model
#'
#' @details
#' Create a plot to display the time series data along with any change points
#' detected by the segmented package
#' 
#' @param tsData  The time series data used to train the model
#' @param model   The trained segmented model
#' @param plotTitle The title for the plot
#' @param plotSubtitle The subtitle for the plot
#'
#' @export
plotSegmented <- function(tsData,
                          model,
                          plotTitle = "Segmented Regression",
                          plotSubtitle = "") {
  colors <- c("Cohort time series" = "black", "Fitted Values" = "blue", "Changepoint" = "red")

  tsData <- tsData %>%
    mutate(
      eventDate = lubridate::as_date(.data$eventDate),
      fitted.values = model$fitted.values)
  
  p <- ggplot2::ggplot(data = tsData, ggplot2::aes(x = .data$eventDate)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$eventCount, color = "Cohort time series")) +
    ggplot2::geom_line(ggplot2::aes(y = .data$eventCount, color = "Cohort time series")) +
    ggplot2::geom_line(ggplot2::aes(y = fitted.values, color = "Fitted Values"), linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, color = "grey40") +
    ggplot2::scale_x_date(date_labels = "%m-%Y") +
    ggplot2::labs(title = plotTitle,
                  subtitle = plotSubtitle, 
                  y = "Event count",
                  x = "Event date") +
    ggplot2::scale_color_manual(name = "Legend", values = colors)
  
  # Plot the change points
  for (i in 1:nrow(model$psi)) {
    psi <- model$psi[i,2]
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(xintercept = psi, color = "Changepoint"), show.legend = TRUE)
  }
  
  return(p)    
}

#' Plots time series data with change points computed by the ocp package 
#'
#' @details
#' Create a plot to display the time series data along with any change points
#' detected by the ocp package
#' 
#' @param tsData  The time series data used to train the model
#' @param model   The trained ocp model
#' @param plotTitle The title for the plot
#' @param plotSubtitle The subtitle for the plot
#'
#' @export
plotOcp <- function(tsData,
                    model,
                    plotTitle = "Bayesian Online Changepoint Detection",
                    plotSubtitle = "") {
  colors <- c("Cohort time series" = "black", "Changepoint" = "red")
  
  tsData <- tsData %>%
    mutate(eventDate = lubridate::as_date(.data$eventDate))
  
  p <- ggplot2::ggplot(data = tsData, ggplot2::aes(x = .data$eventDate)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$eventCount, color = "Cohort time series")) +
    ggplot2::geom_line(ggplot2::aes(y = .data$eventCount, color = "Cohort time series")) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40") +
    ggplot2::scale_x_date(date_labels = "%m-%Y") +
    ggplot2::labs(title = plotTitle,
                  subtitle = plotSubtitle, 
                  y = "Event count",
                  x = "Event date") +
    ggplot2::scale_color_manual(name = "Legend", values = colors)
  
  # Plot the change points
  cpts = model$changepoint_lists[[1]]
  cpts=cpts[[1]][-1]
  
  for (i in 1:length(cpts)) {
    psi <- tsData[cpts[i],]$eventDate
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(xintercept = psi, color = "Changepoint"), show.legend = TRUE)
  }
  
  return(p)    
}