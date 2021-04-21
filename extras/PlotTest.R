library(dplyr)
resultModel <- readRDS("D:/TimeSeriesAnalysis/Run/Results/linear_t1000_e2001_w1_cdm_premier_v1481.rds")

ggplotCpd <- function(trendData, segmentedModel, xAxisLabel = "Time", yAxisLabel = "Event Count (n)") {
  # get n estimated cpts
  ncpts_est <- segmentedModel$ncpts
  # get cpts
  cptsInitial <- segmentedModel$model$psi[1:ncpts_est, 1]  # first column is the initial cpts, one cpt per row
  cptsSegmented <- segmentedModel$model$psi[1:ncpts_est, 2]  # secnd column is the final   cpts, one cpt per row
  confInterval <- segmentedModel$confint
  my.fitted <- stats::fitted(segmentedModel$model)
  my.model <- data.frame(eventCount = my.fitted, id = trendData$id)
  
  plot <- ggplot2::ggplot(trendData, ggplot2::aes(x = id, y = eventCount)) +
    ggplot2::geom_point(shape = 16, size = 2, color = "black") +
    ggplot2::scale_x_continuous(breaks = as.vector(trendData$id), labels = as.vector(trendData$yearMonth)) +
    ggplot2::labs(x = xAxisLabel, y = yAxisLabel) +
    ggplot2::geom_line(data = my.model, color = "blue", ggplot2::aes(x = id, y = eventCount))
  
  # Plot the initial change point specified
  plot <- plot + ggplot2::geom_vline(xintercept=cptsInitial, linetype="longdash", color="gray", size = 1)
  
  # add estimated cpts in red
  for (cp in 1:length(cptsSegmented)) {
    # Add a vertical line and data point for the breakpoint
    bp <- round(cptsSegmented[cp])
    breakpoint <- data.frame(id = bp, eventCount = my.fitted[bp])
    plot <- plot + ggplot2::geom_point(data = breakpoint, color = "red", size = 3, shape=17) +
      #ggplot2::geom_vline(xintercept=bp, linetype="longdash", color="red", size = 1) +
      # Add the confidence interval for the change point
      #ggplot2::geom_pointrange(orientation="x",ggplot2::aes(ymin=confInterval[2],ymax=confInterval[3],color="red"))
      #ggplot2::geom_hline(ggplot2::aes(xmin=confInterval[2],xmax=confInterval[3],color="red"))
      if (!is.null(confInterval)) {
        ggplot2::annotate("pointrange", x = breakpoint$id, y = breakpoint$eventCount, xmin = max(1,confInterval[2]), xmax = round(confInterval[3]),
                          colour = "red", linetype="longdash", size = .5, alpha = .5)
      }
  }  
  
  return(plot)
}

cpdSegmentedModel <- resultModel$o.seg.1
p <- ggplotCpd(resultModel$trendData, cpdSegmentedModel)
p


# # GGplot approach ------------------------------
# library(dplyr)
# dexTrendMod <- trendsByMonthYear[trendsByMonthYear$cohortDefinitionId == 1000 & 
#                                 trendsByMonthYear$eventCohortDefinitionId == 2000 &
#                                 trendsByMonthYear$windowId == 1 & 
#                                 trendsByMonthYear$databaseId == 'cdm_optum_ehr_covid_v1547',]
# dexTrendMod <- dexTrendMod[order(dexTrend$year, dexTrend$month), ]
# dexTrendMod$monthYear <- paste(dexTrendMod$month, dexTrendMod$year, sep="-")
# #dexTrendMod$monthYear <- factor(dexTrendMod$monthYear,levels = unlist(dexTrendMod$monthYear))
# dexTrendMod <- dexTrendMod %>% mutate(id = row_number())
# print(dexTrendMod)
# 
# # fit regression model
# o <- stats::lm(eventCount ~ id, data = dexTrendMod)
# initcp <- 4
# o.seg.1 <- segmented::segmented(o, seg.Z = ~id, psi = initcp, control = segmented::seg.control(display = FALSE)) 
# o.seg.fix <- segmented::segmented(o, seg.Z = ~id, psi = initcp, npsi = 1, control = segmented::seg.control(display = TRUE,
#                                                                                                                   it.max = 100,
#                                                                                                                   n.boot = 100),
#                                   fixed.psi = initcp)  # fix cp to initial user provided
# 
# summary(o.seg.1)
# my.coef <- coef(o.seg.1)
# my.fitted <- fitted(o.seg.1)
# my.model <- data.frame(eventCount = my.fitted, id = dexTrendMod$id)
# #ggplot(my.model, aes(x = id, y = eventCount)) + geom_line() # Check the model
# 
# # get n estimated cpts
# ncpts_est <- nrow(o.seg.1$psi)  # one estimated cpt in one row
# # get cpts
# cpts_initial <- o.seg.1$psi[1:ncpts_est, 1]  # first column is the initial cpts, one cpt per row
# cpts_segmented <- o.seg.1$psi[1:ncpts_est, 2]  # secnd column is the final   cpts, one cpt per row
# confInterval <- segmented::confint.segmented(o.seg.1)
# 
# 
# plot <- ggplot2::ggplot(dexTrendMod, ggplot2::aes(x = id, y = eventCount)) +
#   ggplot2::geom_point(shape = 16, size = 2, color = "black") +
#   ggplot2::scale_x_continuous(breaks = as.vector(dexTrendMod$id), labels = as.vector(dexTrendMod$monthYear)) +
#   ggplot2::labs(x = "Time", y = "Drug Exposures (n)") +
#   ggplot2::geom_line(data = my.model, color = "blue", ggplot2::aes(x = id, y = eventCount))
# 
# # Plot the initial change point specified
# plot <- plot + ggplot2::geom_vline(xintercept=cpts_initial, linetype="longdash", color="gray", size = 1)
# 
# # add estimated cpts in red
# for (cp in 1:length(cpts_segmented)) {
#   # Add a vertical line and data point for the breakpoint
#   bp <- round(cpts_segmented[cp])
#   breakpoint <- data.frame(id = bp, eventCount = my.fitted[bp])
#   plot <- plot + ggplot2::geom_point(data = breakpoint, color = "red", size = 3, shape=17) +
#     #ggplot2::geom_vline(xintercept=bp, linetype="longdash", color="red", size = 1) +
#     # Add the confidence interval for the change point
#     #ggplot2::geom_pointrange(orientation="x",ggplot2::aes(ymin=confInterval[2],ymax=confInterval[3],color="red"))
#     #ggplot2::geom_hline(ggplot2::aes(xmin=confInterval[2],xmax=confInterval[3],color="red"))
#     ggplot2::annotate("pointrange", x = breakpoint$id, y = breakpoint$eventCount, xmin = max(1,confInterval[2]), xmax = round(confInterval[3]),
#                       colour = "red", linetype="longdash", size = .5, alpha = .5)
# }  
# 
# plot
