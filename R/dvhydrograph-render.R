dvhydrographPlot <- function(data) {
  plot_object <- createDvhydrographPlot(data)
  return(plot_object)
}

createDvhydrographPlot <- function(data) {
  
  dvData <- parseDVData(data)
  isInverted <- data$reportMetadata$isInverted
  
  #semantics for min/max are swapped on inverted plots
  maxLabel <- "Max. Instantaneous"
  minLabel <- "Min. Instantaneous";
  if(isInverted) {
    maxLabel <- "Min. Instantaneous"
    minLabel <- "Max. Instantaneous";
  }
  
  if(anyDataExist(dvData)){
    dvInfo <- parseDVSupplemental(data, dvData)
    startDate <- formatDates(data$reportMetadata$startDate) 
    endDate <- formatDates(data$reportMetadata$endDate) + hours(23) + minutes(45)
    plotDates <- seq(startDate, endDate, by=7*24*60*60)
    
    plot_object <- gsplot(ylog = dvInfo$logAxis, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = isInverted) %>%
      view(xlim = c(startDate, endDate)) %>%
      legend(location = "below", cex = 0.8, y.intersp = 1.5) %>%
      title(
        main = "DV Hydrograph",
        ylab = paste0(data$firstDownChain$type, ", ", data$firstDownChain$units),
        line = 3
      )
    
    # for non-approval-bar objects
    for (i in grep("^appr_", names(dvData), invert = TRUE)) {
      dvStyles <- getDvStyle(dvData[i], dvInfo, maxLabel = maxLabel, minLabel = minLabel)
      for (j in names(dvStyles)) {
        dvStyles[[j]] <- extendStep(dvStyles[[j]], endDate = endDate)
        plot_object <- do.call(names(dvStyles[j]), append(list(object = plot_object), dvStyles[[j]]))
      }
    }

    plot_object <- rm.duplicate.legend.items(plot_object)
    
    # custom gridlines below approval bar
    plot_object <- plot_object %>% 
      abline(v=seq(from=startDate, to=endDate, by="days"), lty=3, col="gray", where='first') %>%
      abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1, where='first')

    # approval bar styles are applied last, because it makes it easier to align
    # them with the top of the x-axis line
    plot_object <- ApplyApprovalBarStyles(plot_object, dvData)
      
    # if the y-axis is inverted
    if (plot_object$side.2$reverse) {
      # re-scale top of y-axis so there is a ~4% margin above highest value
      # (i.e., sort of a DIY, "yaxs = 'r'", because we have to use "yaxs = 'i'
      # above, when plot_object is created, to avoid y-axis auto-sizing chaos
      # induced by aligning approval bars with x-axis line)
      plot_object$side.2$lim[1] <- 0.96 * plot_object$side.2$lim[1]
    }
    else {
      # ...as well, but for non-inverted y-axis situation
      plot_object$side.2$lim[1] <- 1.04 * plot_object$side.2$lim[1]
    }
      
    return(plot_object)
  }
  else {
    plot_object <- NULL
  }
}

createRefPlot <- function(data, series) {
  
  # capitalize the reference series name for plot titles
  ref_name_letters <- strsplit(series, "")[[1]]
  ref_name_letters[1] <- LETTERS[which(letters == ref_name_letters[1])]
  ref_name_capital <- paste0(ref_name_letters, collapse = "")
  
  ref_name <- paste0(series, "ReferenceTimeSeries")
  
  if (!length(data[[ref_name]]$points)==0) {
    
    refData <- parseRefData(data, series)
    isInverted <- data$reportMetadata$isInverted
    logAxis <- isLogged(data, refData, ref_name)
    
    startDate <- formatDates(data$reportMetadata$startDate)
    endDate <- formatDates(data$reportMetadata$endDate) + hours(23) + minutes(45)
    plotDates <- seq(startDate, endDate, by=7*24*60*60)
    
    plot_object <- gsplot(ylog = logAxis, yaxs = 'i') %>%
      grid(nx = NA, ny = NULL, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = isInverted) %>%
      view(xlim = c(startDate, endDate)) %>%
      title(
        main = paste(ref_name_capital, "Reference Time Series"),
        ylab = paste(data[[ref_name]]$type, data[[ref_name]]$units),
        line = 3
      ) %>%
      legend(location = "below", cex = 0.8, y.intersp = 1.5)
    
    # for non-approval-bar objects
    for (i in grep("^appr_", names(refData), invert = TRUE)) {
      refStyles <- getDvStyle(refData[i])
      for (j in seq_len(length(refStyles))) {
        plot_object <- do.call(names(refStyles[j]), append(list(object = plot_object), refStyles[[j]]))
      }
    }

    plot_object <- ApplyApprovalBarStyles(plot_object, refData)
    
    plot_object <- rm.duplicate.legend.items(plot_object)
    
    plot_object <- plot_object %>% 
      abline(v=seq(from=startDate, to=endDate, by="days"), lty=3, col="gray", where='first') %>%
      abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1, where='first')
    
    return(plot_object)
  }
}

#' Apply styles (and some properties) to approval bar rectangles.
#' @param object A gsplot, plot object.
#' @param data A list of gsplot objects to display on the plot.
#' @return gsplot object with approval bar rectangle styles applied.
ApplyApprovalBarStyles <- function(object, data) {
  # calculate approval bar rectangle, vertical extent
  ybottom <- ApprovalBarYBottom(
    object$side.2$lim, object$global$par$ylog, object$side.2$reverse
  )
  ytop <- ApprovalBarYTop(
    object$side.2$lim, object$global$par$ylog, object$side.2$reverse
  )
  
  # for any approval intervals present...
  for (i in grep("^appr_", names(data))) {
    # look up style
    approvalBarStyles <- getApprovalBarStyle(data[i], ybottom, ytop)
    for (j in names(approvalBarStyles)) {
      # apply the styles
      object <- do.call(names(approvalBarStyles[j]),
                        append(list(object = object), approvalBarStyles[[j]]))
    }
  }
  return(object)
}

#' Compute top position of approval bars.
#' @param lim The y-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the y-axis is log_10 scale:
#'             TRUE => log_10; FALSE => linear.
#' @param reverse A Boolean, indicating whether the y-axis is inverted:
#'                TRUE => inverted y-axis; FALSE => not inverted.
#' @return Approval bar, vertical top extent, in world coordinates.
ApprovalBarYTop <- function(lim, ylog, reverse) {
  return(ApprovalBarY(lim, ylog, reverse, 0.0245))
}

#' Compute bottom position of approval bars.
#' @param lim The y-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the y-axis is log_10 scale:
#'             TRUE => log_10; FALSE => linear.
#' @param reverse A Boolean, indicating whether the y-axis is inverted:
#'                TRUE => inverted y-axis; FALSE => not inverted.
#' @return Approval bar, vertical bottom extent, in world coordinates.
ApprovalBarYBottom <- function(lim, ylog, reverse) {
  return(ApprovalBarY(lim, ylog, reverse, 0.04))
}

#' Compute top or bottom vertical position of approval bars.
#' @param lim The y-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the y-axis is log_10 scale:
#'             TRUE => log_10; FALSE => linear.
#' @param reverse A Boolean, indicating whether the y-axis is inverted:
#'                TRUE => inverted y-axis; FALSE => not inverted.
#' @param ratio A scaling ratio to adjust top or bottom of approval bar rectangle.
#' @return Approval bar, top or bottom y-axis point, in world coordinates.
ApprovalBarY <- function(lim, ylog = NULL, reverse, ratio) {
  if (is.null(ylog)) {
    # presume the semantics of NULL as FALSE, which may or not be correct, but 
    # keeps the code from terminating here
    ylog <- FALSE
  }
  
  e.0 <- lim[1]
  e.1 <- lim[2]
  
  # if this is a log10 y-axis
  if (ylog) {
    # if y-axis is inverted
    if (reverse) {
      y <- 10^(log10(e.1) + ratio * (log10(e.1) - log10(e.0)))
    }
    else {
      y <- 10^(log10(e.0) - ratio * (log10(e.1) - log10(e.0)))
    }
  }
  else {
    if (reverse) {
      y <- e.1 + ratio * (e.1 - e.0)
    }
    else {
      y <- e.0 - ratio * (e.1 - e.0)
    }
  }
  
  return(y)
}
