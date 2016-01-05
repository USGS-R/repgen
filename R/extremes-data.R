#'@title create a flat text 'extremes table' type output table
#'@param rawData extremes report json string
#'@importFrom dplyr mutate
#'@return string table
#'@export

extremesTable <- function(rawData){
  data <- applyQualifiers(rawData)
  
  primaryLabel <- getReportMetadata(rawData,'primaryLabel')
  primaryParameter <- getReportMetadata(rawData,'primaryParameter')
  primaryUnit <- getReportMetadata(rawData,'primaryUnit')
  
  upchainLabel <- getReportMetadata(rawData,'upchainLabel')
  upchainParameter <- getReportMetadata(rawData,'upchainParameter')
  upchainUnit <- getReportMetadata(rawData,'upchainUnit')
  
  dvLabel <- getReportMetadata(rawData,'dvLabel')
  dvParameter <- getReportMetadata(rawData,'dvParameter')
  dvComputation <- getReportMetadata(rawData,'dvComputation')
  dvUnit <- getReportMetadata(rawData,'dvUnit')
  
  columnNames <- c("",
                   "Date", 
                   "Time", 
                   paste(primaryParameter, " (", primaryUnit, ")"), 
                   paste(upchainParameter, " (", upchainUnit, ")")
                   )
  
  orderedRowNames <- c(paste("Max Inst ", upchainParameter, " and corresponding ", primaryParameter), 
                       paste("Max Inst ", primaryParameter, " and corresponding ", upchainParameter),
                       paste("Max Daily ", dvComputation, " ", dvParameter),
                       paste("Min Inst ", upchainParameter, " and corresponding ", primaryParameter),
                       paste("Min Inst ", primaryParameter, " and corresponding ", upchainParameter),
                       paste("Min Daily ", dvComputation, " ", dvParameter)
                       )
  
  index <- which(names(data) %in% c("upchain", "primary", "dv")) 
  results <- list()
  
  for (i in index) {  
    
    subset <- data[[i]][which(names(data[[i]])%in%c("min","max"))]
    
    min.max <- lapply(subset, function(x) {

      #Formatting for times/dates
      dateTime <- t(data.frame(strsplit(x$points$time, split="[T]")))
      dateTime[,1] <- strftime(dateTime[,1], "%m-%d-%Y")
      
      #Break apart, format dates/times, put back together.
      timeFormatting <- sapply(dateTime[,2], function(s) {
        m <- regexec("([^-+]+)([+-].*)", s)
        splitTime <- unlist(regmatches(s, m))[2:3]
        return(splitTime)
      })
      timeFormatting[1,] <- sapply(timeFormatting[1,], function(s) sub(".000","",s))
      timeFormatting[2,] <- paste0(" (UTC ",timeFormatting[2,], ")")
      timeFormatting <-  paste(timeFormatting[1,],timeFormatting[2,])
      
      if(any(names(x) == "relatedPrimary")) {
        
        if (is.null(x$relatedPrimary$value)) {
          primary <-"N/A" 
        }
        else {
          primary <- x$relatedPrimary$value
        }
        
        if (is.null(x$points$value)) {
          upchain <- "N/A"
        }
        else {
          upchain <- x$points$value
        }
      } else if (any(names(x) == "relatedUpchain")) {
        if (is.null(x$relatedUpchain$value)) {
          upchain <- "N/A"
        }
        else {
          upchain  <- x$relatedUpchain$value
        }
        if (is.null(x$points$value)) {
          primary <- "N/A"
        }
        else {
          primary <- x$points$value  
        }
        
      } else {
        if (is.null(x$points$value)) {
          primary <- "N/A"
        }
        else {
          primary <- x$points$value  
        }
        upchain  <- "N/A"
      }
      
      data.frame(dateTime[,1], timeFormatting, primary, upchain, stringsAsFactors = FALSE)
      
      
    })
    
    names(min.max) <- paste0("data$", names(data)[i], "$", names(subset))
    results <- append(results, min.max) 
    
  }
  
  results <- orderMaxMin(results, data$reportMetadata$isInverted)

  #Change column and row names to their correct forms and add them into the dataframe.
  toRet <- data.frame()
  for(i in 1:length(results)){
    toAdd <- cbind(c(orderedRowNames[i],rep("",nrow(results[[i]])-1)),results[[i]]) 
    colnames(toAdd) <- columnNames
    rownames(toAdd) <- NULL

    toRet <- rbind(toRet,toAdd)
  }
  
  return(toRet)
}

applyQualifiers <- function(data) {
  consolidatedQualifiers <- list(
    primary=data$primary$qualifiers, 
    upchain=data$upchain$qualifiers,
    dv=data$dv$qualifiers)
  
  return(sapply(data, function(x) {
    if(! is.null(x$qualifiers)) {
      x$max$points <- applyQualifiersToValues(x$max$points, x$qualifiers)
      x$min$points <- applyQualifiersToValues(x$min$points, x$qualifiers)
      x$max$relatedUpchain <- applyQualifiersToValues(x$max$relatedUpchain, consolidatedQualifiers$upchain)
      x$min$relatedUpchain <- applyQualifiersToValues(x$min$relatedUpchain, consolidatedQualifiers$upchain)
      x$max$relatedPrimary <- applyQualifiersToValues(x$max$relatedPrimary, consolidatedQualifiers$primary)
      x$min$relatedPrimary <- applyQualifiersToValues(x$min$relatedPrimary, consolidatedQualifiers$primary)
    }
    return(x)
  }))
}

applyQualifiersToValues <- function(points, qualifiers) {
  if(identical("",points)){
    points <- NULL
  }
  if(is.null(points)) {
    return(points)
  }
  
  getQualifierString <- function(p) {
    builtQualifiers <- ""
    if(length(qualifiers) > 0) {
      for(i in 1:nrow(qualifiers)) {
        q <- qualifiers[i,]
        startDate <- q$startDate
        endDate <- q$endDate
        if(p$time > startDate & p$time < endDate) {
          builtQualifiers <- paste0(builtQualifiers, q$code, ",")
        }
      }
      strLength <- nchar(builtQualifiers)
      if(strLength > 0) {
        builtQualifiers <- substr(builtQualifiers, 1, strLength-1)
      }
    }
    return(builtQualifiers)
  }
  
  points <- mutate(points, 
         value = paste(getQualifierString(points), points$value))
  return(points)
}

flattenParam <- function(param){
  baseParam <- strsplit(gsub("([A-Z])", " \\1", param[1]), " ")[[1]]
  param <- paste(unique(c(baseParam, param[-1])), collapse='')
  return(param)
}

orderMaxMin <- function(results, isInverted){
  
  if(isInverted){
    maximums <- results[grep("min", names(results))]
    minimums <- results[grep("max", names(results))]
  } else {
    maximums <- results[grep("max", names(results))]
    minimums <- results[grep("min", names(results))]
  }
  
  maximums_index <- c(grep("upchain", names(maximums)), 
                      grep("primary", names(maximums)), 
                      grep("dv", names(maximums)))
  
  minimums_index <- c(grep("upchain", names(minimums)), 
                      grep("primary", names(minimums)), 
                      grep("dv", names(minimums)))
  
  maximums <- maximums[maximums_index]
  minimums <- minimums[minimums_index]
  
  results <- list()
  results <- append(results, maximums)
  results <- append(results, minimums)
  
  return(results)
}
