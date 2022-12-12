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
  segmentedLineColors <- c("Segmented Regression Line 1" = "#49FF33",
                           "Segmented Regression Line 2" = "#3362FF", 
                           "Segmented Regression Line 3" = "#33FFFE")
  colors <- c(segmentedLineColors,
              "Cohort time series" = "black", 
              "Changepoint" = "red")

  # Taken from: https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/
  ## grad the inverse link function
  ilink <- family(model)$linkinv
  ## add fit and se.fit on the **link** scale
  fittedData <- setNames(dplyr::as_tibble(predict(model, se.fit = TRUE)[1:2]),c('fitted.value','se.link'))
  ## create the interval and backtransform
  fittedData <- mutate(fittedData,
                       fitted.value  = ilink(fitted.value),
                       upr = ilink(fitted.value + (2 * se.link)),
                       lwr = ilink(fitted.value - (2 * se.link)))
  # Add the 
  tsData <- tsData %>%
    mutate(
      eventDate = lubridate::as_date(.data$eventDate)) %>%
    bind_cols(fittedData)

  p <- ggplot2::ggplot(data = tsData, ggplot2::aes(x = .data$eventDate)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$eventCount, color = "Cohort time series")) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, 
                                      ymax = upr, 
                                      y = .data$fitted.value), 
                         alpha = 0.05) +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept = 0, color = "grey40") +
    ggplot2::scale_x_date(date_labels = "%m-%Y") +
    ggplot2::labs(title = plotTitle,
                  subtitle = plotSubtitle, 
                  y = "Event count",
                  x = "Event date") +
    ggplot2::scale_fill_manual(name = "Legend", values = colors, aesthetics = c("colour", "fill")) +
    ggplot2::theme(legend.position = "bottom")
  
  findY <- function(model, changePointIndex, term) {
    psi <- model$psi[changePointIndex,2]
    d <- data.frame(a = psi)
    names(d) <- term
    psiY <- segmented::broken.line(ogg = model,
                                   term = d,
                                   se.fit = FALSE)[[1]]
    invisible(psiY)
  }
  
  # Plot the change points & segmented regression lines
  for (i in 1:nrow(model$psi)) {
    # Get the changepoint term from the model
    term <- model$nameUV$Z
    # Get the estimated change point
    psi <- model$psi[i,2]
    psi.date <- lubridate::as_date(psi)
    d <- data.frame(a = psi)
    names(d) <- term
    psiY <- findY(model = model,
                  changePointIndex = i,
                  term = term)
    
    # Get the estimated change point confidence interval
    cpConfInt <- confint(object = model)[i,]
    
    # Define the line data before (line1) the change point
    xMinLine1 <- lubridate::as_date(ifelse(i == 1, model$rangeZ[[1]], model$psi[i-1,2]))
    xEndLine1 <- psi.date
    yMinLine1 <- ifelse(i == 1, model$fitted.values[[1]], findY(model = model,
                                                                changePointIndex = i-1,
                                                                term = term))
    yEndLine1 <- psiY
    dfLineSegment <- data.frame(xmin = xMinLine1, 
                                xend = xEndLine1, 
                                ymin = yMinLine1,
                                yend = yEndLine1)
    
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(xintercept = psi.date, color = "Changepoint"), linetype = "dashed", show.legend = TRUE) +
      ggplot2::geom_point(ggplot2::aes(x = psi.date, y = psiY, color = "Changepoint"), shape = 1, size = 5) + 
      ggplot2::geom_errorbar(ggplot2::aes(xmin = lubridate::as_date(cpConfInt[2]), 
                                          xmax = lubridate::as_date(cpConfInt[3]), 
                                          y = psiY,
                                          color = "Changepoint")) +
      ggplot2::geom_segment(ggplot2::aes(x = xmin, y = ymin, xend = xend, yend = yend, colour = names(segmentedLineColors[(i-1)%%length(segmentedLineColors)+1])), data = dfLineSegment)
    
    # Plot the line after the change point (line2)
    if (i == nrow(model$psi)) {
      xMinLine2 <- psi.date
      xEndLine2 <- lubridate::as_date(model$rangeZ[[2]])
      yMinLine2 <- psiY
      yEndLine2 <- model$fitted.values[[length(model$fitted.values)]]
      dfLineSegment <- data.frame(xmin = xMinLine2,
                                  xend = xEndLine2,
                                  ymin = yMinLine2,
                                  yend = yEndLine2)
      p <- p +
        ggplot2::geom_segment(ggplot2::aes(x = xmin, y = ymin, xend = xend, yend = yend, colour = names(segmentedLineColors[i%%length(segmentedLineColors)+1])), data = dfLineSegment)
    }
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