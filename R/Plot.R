#' Plots time series data
#'
#' @details
#' Create a plot to display the time series data and optionally formats the plot.
#' This function is used by the other plotting functions in this package
#' which will apply the formatting itself.
#'
#'
#' @param tsData  The time series data used to train the model
#' @param applyFormatting When TRUE, a default formatting will be applied. Default is FALSE
#' @param plotTitle The title for the plot
#' @param plotSubtitle The subtitle for the plot
#'
#' @export
plotTimeSeries <- function(tsData,
                           applyFormatting = FALSE,
                           plotTitle = "Time series",
                           plotSubtitle = "") {
  tsData <- tsData %>%
    mutate(eventDate = lubridate::as_date(.data$eventDate))
  p <- ggplot2::ggplot(
    data = tsData,
    ggplot2::aes(
      x = .data$eventDate,
      y = .data$eventCount
    )
  )
  if (applyFormatting) {
    p <- p + ggplot2::geom_point()
    p <- .formatPlot(
      p = p,
      plotTitle = plotTitle,
      plotSubtitle = plotSubtitle
    )
  }

  p
}

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
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assert_true(!is.null(model), add = errorMessages)
  checkmate::assert_data_frame(x = tsData, min.rows = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  segmentedLineColors <- c(
    "Segmented Regression Line 1" = "#49FF33",
    "Segmented Regression Line 2" = "#3362FF",
    "Segmented Regression Line 3" = "#33FFFE"
  )
  colors <- c(segmentedLineColors,
    "Cohort time series" = "black",
    "Est. Changepoint" = "red",
    "Pre-specified Changepoint" = "#99d594"
  )

  # When model$nameUV is not null, the segmented
  # package can obtain the fitted values from the
  # segmented model. Otherwise compute the fitted
  # values and standard errors to create the plot
  # with 95% confidence intervals
  if (!is.null(model$nameUV)) {
    fittedData <- segmented::broken.line(ogg = model)
  } else {
    fittedData <- stats::predict(model, se.fit = TRUE)
  }
  fittedData <- tibble(
    fitted.value = fittedData$fit,
    upr = fittedData$fit + (1.96 * fittedData$se.fit),
    lwr = fittedData$fit - (1.96 * fittedData$se.fit)
  )

  # Add the original time series data along with the fitted data
  tsData <- tsData %>%
    mutate(
      eventDate = lubridate::as_date(.data$eventDate)
    ) %>%
    bind_cols(fittedData)

  p <- plotTimeSeries(tsData, applyFormatting = FALSE) +
    ggplot2::geom_point(ggplot2::aes(y = .data$eventCount, color = "Cohort time series")) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$lwr,
        ymax = .data$upr,
        y = .data$fitted.value
      ),
      alpha = 0.05
    ) +
    ggplot2::scale_fill_manual(name = "Legend", values = colors, aesthetics = c("colour", "fill"))

  findY <- function(model, changePointIndex, term) {
    psi <- model$psi[changePointIndex, 2]
    d <- data.frame(a = psi)
    names(d) <- term
    psiY <- segmented::broken.line(
      ogg = model,
      term = d,
      se.fit = FALSE
    )[[1]]
    invisible(psiY)
  }

  getFixedChangepointNames <- function(indexU) {
    return(grepl("^U[0-9]*.fixed", x = names(indexU)))
  }

  getFixedChangepoints <- function(indexU) {
    return(indexU[getFixedChangepointNames(indexU)])
  }


  # Obtain the change points and segmented regression lines to plot
  dfLineSegment <- data.frame()
  dfEstimatedChangepoints <- data.frame()
  dfPrespecifiedChangepoints <- data.frame()

  # Get all fixed pre-specified
  if (is.null(model$nameUV)) {
    # In this situation, the model did not estimate any change points
    # so we can interrogate the model to identify any pre-specified
    # changepoints
    for (i in 1:nrow(model$psi)) {
      psi <- model$psi[i, 2]
      psi.date <- lubridate::as_date(psi)
      psiY <- tsData[tsData$eventDate == psi.date, ]$eventCount
      dfPrespecifiedChangepoints <- rbind(
        dfPrespecifiedChangepoints,
        data.frame(
          x = psi.date,
          y = psiY
        )
      )

      # In this scenario, 1 or more pre-specified change points may be specified.
      # Create a single line segment to show the fit of the linear model
      if (i == 1) {
        # Define the line data before (line1) the change point
        xMinLine1 <- tsData$eventDate[1]
        xEndLine1 <- tsData$eventDate[nrow(tsData)]
        yMinLine1 <- model$fitted.values[1]
        yEndLine1 <- model$fitted.values[nrow(tsData)]
        dfLineSegment <- rbind(
          dfLineSegment,
          data.frame(
            xmin = xMinLine1,
            xend = xEndLine1,
            ymin = yMinLine1,
            yend = yEndLine1,
            lineColour = names(segmentedLineColors[(i - 1) %% length(segmentedLineColors) + 1])
          )
        )
      }
    }
  }

  # Another place pre-specified change points are included is in the indexU
  # field so check this as well. When indexU exists, the model produced
  # 1 or more estimated change points and may have a pre-speciifed change point
  # if the fixed.psi argument was used.
  if (!is.null(model$indexU)) {
    fixedChangepoints <- getFixedChangepoints(model$indexU[[1]])
    if (length(fixedChangepoints) > 0) {
      for (i in 1:length(fixedChangepoints)) {
        psi <- fixedChangepoints[i]
        psi.date <- lubridate::as_date(psi)
        psiY <- tsData[tsData$eventDate == psi.date, ]$eventCount
        dfPrespecifiedChangepoints <- rbind(
          dfPrespecifiedChangepoints,
          data.frame(
            x = psi.date,
            y = psiY
          )
        )
      }
    }
  }

  # Iterate over the estimated change points
  if (nrow(model$psi) > 0) {
    # Assemble the data frames used to plot
    # the change points & segmented regression lines
    for (i in 1:nrow(model$psi)) {
      # Get the changepoint term from the model
      term <- model$nameUV$Z
      # Get the change point
      psi <- model$psi[i, 2]
      psi.date <- lubridate::as_date(psi)

      # Determine the type of changepoint based on the presence/absence
      # of the term in the model
      if (!is.null(term)) {
        # This is an estimated change point
        d <- data.frame(a = psi)
        names(d) <- term
        psiY <- findY(
          model = model,
          changePointIndex = i,
          term = term
        )

        # Get the estimated change point confidence interval
        cpConfInt <- confint(object = model)[i, ]

        # Add the change point to the data.frame
        dfEstimatedChangepoints <- rbind(
          dfEstimatedChangepoints,
          data.frame(
            x = psi.date,
            y = psiY,
            ci_lb = lubridate::as_date(cpConfInt[2]),
            ci_ub = lubridate::as_date(cpConfInt[3])
          )
        )
        # Define the line data before (line1) the change point
        xMinLine1 <- lubridate::as_date(ifelse(i == 1, model$rangeZ[[1]], model$psi[i - 1, 2]))
        xEndLine1 <- psi.date
        yMinLine1 <- ifelse(i == 1, model$fitted.values[[1]], findY(
          model = model,
          changePointIndex = i - 1,
          term = term
        ))
        yEndLine1 <- psiY
        dfLineSegment <- rbind(
          dfLineSegment,
          data.frame(
            xmin = xMinLine1,
            xend = xEndLine1,
            ymin = yMinLine1,
            yend = yEndLine1,
            lineColour = names(segmentedLineColors[(i - 1) %% length(segmentedLineColors) + 1])
          )
        )


        # Plot the line after the change point (line2)
        if (i == nrow(model$psi)) {
          xMinLine2 <- psi.date
          xEndLine2 <- lubridate::as_date(model$rangeZ[[2]])
          yMinLine2 <- psiY
          yEndLine2 <- model$fitted.values[[length(model$fitted.values)]]
          dfLineSegment <- rbind(
            dfLineSegment,
            data.frame(
              xmin = xMinLine2,
              xend = xEndLine2,
              ymin = yMinLine2,
              yend = yEndLine2,
              lineColour = names(segmentedLineColors[i %% length(segmentedLineColors) + 1])
            )
          )
        }
      }
    }
  }

  # Plot the pre-specified change points
  if (nrow(dfPrespecifiedChangepoints) > 0) {
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(xintercept = x, color = "Pre-specified Changepoint"), linetype = "dashed", show.legend = TRUE, data = dfPrespecifiedChangepoints) +
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = "Pre-specified Changepoint"), shape = 18, size = 5, data = dfPrespecifiedChangepoints)
  }

  # Plot the estimated change points
  if (nrow(dfEstimatedChangepoints) > 0) {
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(xintercept = x, color = "Est. Changepoint"), linetype = "dashed", show.legend = TRUE, data = dfEstimatedChangepoints) +
      ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = "Est. Changepoint"), shape = 1, size = 5, data = dfEstimatedChangepoints) +
      ggplot2::geom_errorbar(ggplot2::aes(
        xmin = ci_lb,
        xmax = ci_ub,
        x = x,
        y = y,
        color = "Est. Changepoint"
      ), data = dfEstimatedChangepoints)
  }

  # Plot the segmented regression lines
  if (nrow(dfLineSegment) > 0) {
    p <- p +
      ggplot2::geom_segment(ggplot2::aes(x = xmin, y = ymin, xend = xend, yend = yend, colour = lineColour), data = dfLineSegment)
  }

  p <- .formatPlot(p, plotTitle, plotSubtitle)
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
  colors <- c("Cohort time series" = "black", "Est. Changepoint" = "red")

  tsData <- tsData %>%
    mutate(eventDate = lubridate::as_date(.data$eventDate))

  p <- plotTimeSeries(tsData, applyFormatting = FALSE) +
    ggplot2::geom_point(ggplot2::aes(y = .data$eventCount, color = "Cohort time series")) +
    ggplot2::geom_line(ggplot2::aes(y = .data$eventCount, color = "Cohort time series")) +
    ggplot2::scale_color_manual(name = "Legend", values = colors)

  # Plot the change points
  cpts <- model$changepoint_lists[[1]]
  cpts <- cpts[[1]][-1]

  for (i in 1:length(cpts)) {
    psi <- tsData[cpts[i], ]$eventDate
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(xintercept = psi, color = "Est. Changepoint"), show.legend = TRUE)
  }

  p <- .formatPlot(p, plotTitle, plotSubtitle)
  return(p)
}


.formatPlot <- function(p, plotTitle, plotSubtitle) {
  p <- p +
    ggplot2::scale_x_date(date_labels = "%m-%Y") +
    ggplot2::labs(
      title = plotTitle,
      subtitle = plotSubtitle,
      y = "Event count",
      x = "Event date"
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept = 0, color = "grey40")

  invisible(p)
}
