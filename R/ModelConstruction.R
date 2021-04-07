#' @export
buildModels <- function(exportFolder, changePointMonth, changePointYear) {
  start <- Sys.time()
  if (!is.numeric(changePointMonth) || !is.numeric(changePointYear)) {
    stop("changePointMonth and changePointYear must contain an integer value.")
  }
  changePointMonth <- as.integer(changePointMonth)
  changePointYear <- as.integer(changePointYear)
  # Load the data either from PreMerged.Rdata or from
  # the .csv file that holds the results
  if (file.exists(file.path(exportFolder, "PreMerged.RData"))) {
    load(file.path(exportFolder, "PreMerged.RData"))
  } else if (file.exists(file.path(exportFolder, getTrendsFileName()))) {
    trendsByMonthYear <- readr::read_csv(file.path(exportFolder, getTrendsFileName()), col_types = readr::cols())
    names(trendsByMonthYear) <- SqlRender::snakeCaseToCamelCase(names(trendsByMonthYear))
  } else {
    errorMsg <- paste0("Results file not found! Expected to find one of these files: \"PreMerged.RData\" or \"", getTrendsFileName(), "\" in \"", exportFolder, "\"")
    stop(errorMsg)
  }
  
  # Format the results a bit to include the year-month 
  # and to ensure any censored values are treated as minCellCount 
  # versus a negative
  trendsByMonthYear$yearMonth <- paste(trendsByMonthYear$year, trendsByMonthYear$month, sep = "-")
  trendsByMonthYear$eventCount <- abs(trendsByMonthYear$eventCount)
  
  # Find the unique combos of target/event/window and use
  # these to build each model
  targetEventWindowCombos <- unique(trendsByMonthYear[,c("cohortDefinitionId", "eventCohortDefinitionId", "windowId", "databaseId")])
  for(i in 1:nrow(targetEventWindowCombos)) {
    cohortDefinitionId <- targetEventWindowCombos$cohortDefinitionId[i]
    eventCohortDefinitionId <- targetEventWindowCombos$eventCohortDefinitionId[i]
    windowId <- targetEventWindowCombos$windowId[i]
    databaseId <- targetEventWindowCombos$databaseId[i]
    tewSubset <- trendsByMonthYear[trendsByMonthYear$cohortDefinitionId == cohortDefinitionId &
                                     trendsByMonthYear$eventCohortDefinitionId == eventCohortDefinitionId &
                                     trendsByMonthYear$windowId == windowId &
                                     trendsByMonthYear$databaseId == databaseId, ]
    tewSubset <- tewSubset[order(tewSubset$year, tewSubset$month),]
    # Identify the index of the change point for building the model
    initcp <- which(tewSubset$month == changePointMonth & tewSubset$year == changePointYear)
    # Build the linear model
    ParallelLogger::logInfo(
      sprintf("Building segmented linear model for cohort: %d, event: %d, window: %d, database: %s", 
              cohortDefinitionId,
              eventCohortDefinitionId,
              windowId,
              databaseId)
    )
    segLinearModel <- createCpdSegmentedLinear(tewSubset$eventCount, 1:nrow(tewSubset), initcp)
    fileName <- createRdsFileName("linear", cohortDefinitionId, eventCohortDefinitionId, windowId, databaseId)
    saveRDS(segLinearModel, file=file.path(exportFolder, fileName))
    # Build the poisson model
    ParallelLogger::logInfo(
      sprintf("Building segmented poisson model for cohort: %d, event: %d, window: %d, database: %s", 
              cohortDefinitionId,
              eventCohortDefinitionId,
              windowId,
              databaseId)
    )
    segPoissonModel <- createCpdSegmentedPoisson(tewSubset$eventCount, 1:nrow(tewSubset), initcp)
    fileName <- createRdsFileName("poisson", cohortDefinitionId, eventCohortDefinitionId, windowId, databaseId)
    saveRDS(segPoissonModel, file=file.path(exportFolder, fileName))
  }
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Model building took",
                                signif(delta, 3),
                                attr(delta, "units")))    
}

createRdsFileName <- function(modelName, cohortDefinitionId, eventCohortDefinitionId, windowId, databaseId) {
  return(sprintf("%s_t%d_e%d_w%d_%s.rds", modelName, cohortDefinitionId, eventCohortDefinitionId, windowId, databaseId))
}

#' @export
createCpdSegmentedPoisson <- function(Samples, Time, initcp) {
  z <- Time
  y <- Samples
  
  # model family
  family = "Poisson"
  
  # fit regression model
  o <- stats::glm(y ~ z, family = poisson)
  return(createCpdSegmented(o, y, z, family, initcp))
}

#' @export
createCpdSegmentedLinear <- function(Samples, Time, initcp){
  z<-Time
  y<-Samples
  
  # model family
  family = "Linear"

  # fit regression model
  o<-stats::lm(y~z)
  return(createCpdSegmented(o, y, z, family, initcp))
}

createCpdSegmented <- function(linearModel, Samples, Time, family, initcp) {
  o <- linearModel
  z <- Time
  y <- Samples
  # create results object
  list4.1    <- list()
  list4.2    <- list()
  list4.auto <- list()
  list4.fix  <- list()
  
  # run segmentation
  # n cpt = 1
  o.seg.1   <-
    segmented::segmented(
      o,
      seg.Z =  ~ z,
      psi = initcp,
      control = segmented::seg.control(display = FALSE)
    )# psi= init cpt
  # n cpt = 2
  #o.seg.2   <-segmented::segmented(o,seg.Z=~z, npsi= 2,        control=seg.control(display=FALSE))
  # n cpt = automatic procedure to estimate breakpoints (starting from K quantiles) Hint: increases number of iterations. Notice: bootstrap restart is not allowed!
  #o.seg.auto <-segmented::segmented(o,seg.Z=~z,psi=list(z=NA), control=seg.control(fix.npsi=FALSE,  tol=1e-7, it.max = 100, K=1, display=TRUE))
  # n cpt = 1, cpt = fixed, user-specified
  o.seg.fix <-
    segmented::segmented(
      o,
      seg.Z =  ~ z,
      psi = initcp,
      npsi = 1,
      control = segmented::seg.control(
        display = TRUE,
        it.max =  100,
        n.boot = 100
      ),
      fixed.psi = initcp
    ) # fix cp to initial user provided

  tryCatch(
    expr = {
      o.seg.2 <-
        segmented::segmented(
          o,
          seg.Z =  ~ z,
          npsi = 2,
          control = seg.control(
            display = FALSE,
            n.boot = 100,
            it.max = 100
          )
        )
      
    },
    error = function(e) {
      # (Optional)
      # Do this if an error is caught...
    },
    warning = function(w) {
      # (Optional)
      # Do this if an warning is caught...
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
      o.seg.2 <- NULL
    }
  )
  
  
  list4.1 <- cpdSave(Samples, Time, o.seg.1,    list4.1, family)
  #list4.2    <- NULL #cpdSave(Samples, Time, o.seg.2,    list4.2, family)
  if (!is.null(o.seg.2)) {
    list4.2 <- cpdSave(Samples, Time, o.seg.2,    list4.2, family)
  }
  list4.auto <- NULL #cpdSave(Samples, Time, o.seg.auto, list4.auto, family)
  list4.fix <- cpdSave(Samples, Time, o.seg.fix,  list4.fix, family)
  
  list_all <- list(list4.1, list4.2, list4.auto, list4.fix)
  
  return(list_all)  
}

# Remove the plot function for now
cpdSave <- function(Samples, Time, o.seg, list4, family) {
  # cpt estimated or not
  est_yes = !is.null(o.seg$psi) & !is.null(o.seg$psi[1, 3])
  #if cpt estimated
  if (est_yes) {
    # if no cpts estimated or if estimated with infinite std error
    segmented::slope(o.seg) #the slopes of the segmented relationship
    # then plot
    #plotCpdSegmented(Time, Samples, o.seg, family)
    # and save plot
    #p <- recordPlot()
  }
  # save results
  list4$models <- o.seg # intercept and slope of all segments
  list4$cpts <- o.seg$psi # inc initial & estimated cpts
  list4$ncpts <- length(o.seg$psi[, 1])
  #list4$plots <- p
  return(list4)
}

#' @export
plotCpdSegmented <- function(Time, Samples, model_cpt, family) {
  # get n estimated cpts
  ncpts_est = nrow(model_cpt$psi) # one estimated cpt in one row
  
  #plot(model_cpt, conf.level=0.95, shade=TRUE,type = "l", xlab = "Month", ylab = "N Drugs", main = paste("Segmented Poisson Regression , ncpt =", ncpts = n, sep =" "), xaxt ="n")# plots regression lines of the two segments using the coeffs returned in model_cpt
  plot(
    1:length(Time), #Time,
    Samples,
    xlab = "Month",
    ylab = "N Drugs",
    main = paste("Segmented", family, "Regression", sep = " "),
    xaxt = "n",
    cex = 1.5,
    pch = 16
  )# add the actual time series, ,type = "l"
  plot(
    model_cpt,
    add = TRUE,
    link = FALSE,
    lwd = 2,
    col = 3:5,
    lty = c(1, 1)
  ) # added fitted line using diff cols for diff segments
  points(model_cpt, link = TRUE, col = 2)# circle the brkpt
  lines(
    model_cpt,
    col = 2,
    pch = 19,
    bottom = FALSE,
    lwd = 2
  ) # add CI for the breakpoint
  
  # change xtick
  
  #mth_lab <- unlist(lapply(Time, month_name))
  #axis(1, at = Time, labels = mth_lab)
  axis(1, at = 1:length(Time), labels = Time)
  
  # get cpts
  cpts_initial   = model_cpt$psi[1:ncpts_est, 1] # first column is the initial cpts, one cpt per row
  cpts_segmented = model_cpt$psi[1:ncpts_est, 2] # secnd column is the final   cpts, one cpt per row
  
  
  #plot cpts
  abline(
    v = cpts_initial,
    col = "grey",
    lwd = 3,
    lty = 2
  ) # plot the initial cpt in grey
  
  for (cp in 1:length(cpts_segmented)) {
    abline(
      v = cpts_segmented[cp],
      col = "red",
      lwd = 3,
      lty = 2
    )
  } # add estimated cpts in red
  
  
  
}

month_name <- Vectorize(
  function(n)
    c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    )[n]
)
