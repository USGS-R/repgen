dvhydrographPlot <- function(data) {
  plot_object <- createDvhydrographPlot(data)
  return(plot_object)
}

#' @importFrom stats na.omit
createDvhydrographPlot <- function(data) {
  options(scipen=8)
  
  dvData <- parseDVData(data)
  isInverted <- fetchReportMetadataField(data, 'isInverted')
  
  if (anyDataExist(dvData)) {
    dvInfo <- parseDVSupplemental(data, dvData)
    startDate <- flexibleTimeParse(data$reportMetadata$startDate, timezone=data$reportMetadata$timezone) 
    endDate <- toEndOfDay(flexibleTimeParse(data$reportMetadata$endDate, timezone=data$reportMetadata$timezone))
    plotDates <- toStartOfDay(seq(startDate, endDate, by = 7 * 24 * 60 * 60))
    
    plot_object <- gsplot(ylog = dvInfo$logAxis, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = isInverted) %>%
      view(xlim = c(startDate, endDate)) %>%
      title(
        ylab = paste0(data$firstDownChain$type, ", ", data$firstDownChain$units),
        line = 3
      )

    plot_object <-
      XAxisLabelStyle(plot_object, startDate, endDate, data$reportMetadata$timezone, plotDates)
    
    # for non-approval-bar objects
    for (i in grep("^appr_", names(dvData), invert = TRUE)) {
      dvStyles <- getDvStyle(dvData[i], dvInfo)
      for (j in names(dvStyles)) {
        dvStyles[[j]] <- extendStep(dvStyles[[j]])
        plot_object <- do.call(names(dvStyles[j]), append(list(object = plot_object), dvStyles[[j]]))
      }
    }

    # approval bar styles are applied last, because it makes it easier to align
    # them with the top of the x-axis line
    plot_object <- ApplyApprovalBarStyles(plot_object, dvData)
    
    plot_object <- rmDuplicateLegendItems(plot_object)
    
    # custom gridlines below approval bar
    plot_object <- plot_object %>% 
      abline(v=seq(from=toStartOfDay(startDate), to=toStartOfDay(endDate), by="days"), lty=3, col="gray", where='first') %>%
      abline(v=seq(from=toStartOfDay(startDate), to=toStartOfDay(endDate), by="weeks"), col="darkgray", lwd=1, where='first')
    
    # patch up top extent of y-axis
    plot_object <- RescaleYTop(plot_object)

    #Legend
    legend_items <- plot_object$legend$legend.auto$legend
    ncol <- ifelse(length(legend_items) > 3, 2, 1)
    leg_lines <- ifelse(ncol==2, ceiling((length(legend_items) - 6)/2), 0) 
    legend_offset <- ifelse(ncol==2, 0.3+(0.05*leg_lines), 0.3)
    plot_object <- legend(plot_object, location="below", cex=0.8, legend_offset=0.2, y.intersp=1.5, ncol=ncol)

    #Add Min/Max labels if we aren't plotting min and max
    minmax_labels <- append(dvData['max_iv_label'], dvData['min_iv_label'])
    line <- 0.33
    for(ml in na.omit(names(minmax_labels))){
      #Extract Timezone
      tzf <- format(as.POSIXct(dvData[[ml]]$time), "%z")
      #Insert ":" before 2nd to last character
      tzf <- sub("([[:digit:]]{2,2})$", ":\\1", tzf)
      formatted_label <- paste0(dvData[[ml]]$legend.name, data$firstDownChain$units, 
                                format(as.POSIXct(dvData[[ml]]$time), " %b %d, %Y %H:%M:%S"), " (UTC ", tzf, ")")
      
      plot_object <- mtext(plot_object, formatted_label, side = 3, axes=FALSE, cex=0.85, line = line, adj = 0)
      
      line <- line + 1
    }
    
    return(plot_object)
  }
  else {
    plot_object <- NULL
  }
}

createRefPlot <- function(data, series, descriptions) {
  
  # capitalize the reference series name for plot titles
  ref_name_letters <- strsplit(series, "")[[1]]
  ref_name_letters[1] <- LETTERS[which(letters == ref_name_letters[1])]
  ref_name_capital <- paste0(ref_name_letters, collapse = "")
    
  if (!length(data[[series]]$points)==0) {
    
    refData <- parseRefData(data, series, descriptions)
    isInverted <- data$reportMetadata$isInverted
    logAxis <- isLogged(refData, data[[series]][['isVolumetricFlow']], fetchReportMetadataField(data, 'excludeZeroNegative'))
    
    startDate <- flexibleTimeParse(data$reportMetadata$startDate, timezone=data$reportMetadata$timezone)
    endDate <- toEndOfDay(flexibleTimeParse(data$reportMetadata$endDate, timezone=data$reportMetadata$timezone))

    plotDates <- toStartOfDay(seq(startDate, endDate, by = 7 * 24 * 60 * 60))

    plot_object <- gsplot(ylog = logAxis, yaxs = 'i') %>%
      grid(nx = NA, ny = NULL, lty = 3, col = "gray") %>%
      axis(2, reverse = isInverted) %>%
      view(xlim = c(startDate, endDate)) %>%
      title(
        main = paste(ref_name_capital, "Reference Time Series"),
        ylab = paste(data[[series]]$type, data[[series]]$units),
        line = 3
      ) %>%
      legend(location = "below", cex = 0.8, y.intersp = 1.5)
    
    plot_object <-
      XAxisLabelStyle(plot_object, startDate, endDate, data$reportMetadata$timezone, plotDates)
    
    # for non-approval-bar objects
    for (i in grep("^appr_", names(refData), invert = TRUE)) {
      refStyles <- getDvStyle(refData[i])
      for (j in seq_len(length(refStyles))) {
        plot_object <- do.call(names(refStyles[j]), append(list(object = plot_object), refStyles[[j]]))
      }
    }
    
    plot_object <- ApplyApprovalBarStyles(plot_object, refData)
    
    plot_object <- rmDuplicateLegendItems(plot_object)
    
    plot_object <- plot_object %>% 
      abline(v=seq(from=startDate, to=endDate, by="days"), lty=3, col="gray", where='first') %>%
      abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1, where='first')
    
    # patch up top extent of y-axis
    plot_object <- RescaleYTop(plot_object)
    
    return(plot_object)
  }
}

#' @importFrom lubridate interval
#' @importFrom lubridate as.period
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate floor_date
#' @importFrom lubridate %m+%
#' @importFrom lubridate %m-%
#' @importFrom lubridate day
#' @importFrom lubridate days
#' @importFrom stats median
XAxisLabelStyle <- function(object, start, end, timezone, plotDates) {
  i <- interval(start, end, tzone = attr(start, timezone))
  
  # if chart interval is less than 1 year
  if (as.period(i) < years(1)) {
    # x-axis
    object <- axis(
      object,
      1, at = plotDates,
      labels = format(plotDates, "%b\n%d"),
      padj = 0.5
    )
  }
  else {
    # if start date day is not the 1st of the month
    if (day(start) != 1) {
      # begin month letter labeling at next adjacent month
      from <- floor_date(start %m+% months(1), "month")
    }
    else {
      from <- start
    }
    
    # if end date day is not the last day of the month
    if (day(end) != days_in_month(end)) {
      # end month letter labeling at preceding adjacent month
      to <- ceiling_date(end %m-% months(1), "month")
    }
    else {
      to <- end
    }
    
    months <-
      seq(
        from = ceiling_date(start, "month"),
        to = floor_date(end, "month"),
        by = "month"
      )
    
    # [start:end] is interval here, because [from:to] above could be abbreviated
    # to omit month-letter-labeling of partial months at beginning/end of x-axis
    years <- seq(from = floor_date(start, "year"), to = floor_date(end, "year"), by = "year")

    object <- axis(object, side = 1, at = months, labels = FALSE) # x-axis
    
    month_label_split <- strsplit(as.character(month(months, label = TRUE)), "")
    text <- unlist(lapply(month_label_split, function(x) { x[1] }))
    
    at.months <- months + days(15) # position label at 15th of month
    
    at.years <-
      do.call(c, lapply(year(years), function(y, plotDates) {
        which.yr.dates <- which(year(plotDates) == y)
        return(median(plotDates[which.yr.dates]))
      }, plotDates = plotDates))
    
    # add year labels to x-axis
    object <- XAxisLabels(object, text, at.months, at.years)
    
    # add vertical lines to delineate calendar year boundaries
    object <- DelineateYearBoundaries(object, years)
  }
  
  return(object)
}