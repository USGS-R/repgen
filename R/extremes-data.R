#'@title create a flat text 'extremes table' type output table
#'@param rawData extremes report json string
#'@importFrom dplyr mutate
#'@return string table
#'@export

extremesTable <- function(rawData){
  data <- applyQualifiers(rawData)
  
  columnNames <- c("","Date", "Time", "Discharge (cfs)", "Gage Height (ft)")
  orderedRowNames <- c("Max inst GH and corresponding Q", 
                       "Max inst Q and corresponding GH",
                       "Max daily Q",
                       "Min inst GH and corresponding Q",
                       "Min inst Q and corresponding GH",
                       "Min daily Q")
  index <- which(names(data) %in% c("gageHeight", "discharge", "dailyDischarge")) 
  results <- list()
  
  for (i in index) {  
    
    subset <- data[[i]][which(names(data[[i]])%in%c("min","max"))]
    
    min.max <- lapply(subset, function(x) {

      #Formatting for times/dates
      dateTime <- t(data.frame(strsplit(x$points$time, split="[T]")))
      dateTime[,1] <- strftime(dateTime[,1], "%m-%d-%Y")
      
      #Break apart, format dates/times, put back together.
      timeFormatting <- sapply(dateTime[,2], function(s) {
        splitTime <- strsplit(s,split="[-]")[[1]]
        splitTime[[2]] <- paste0("-", splitTime[[2]])
        return(splitTime)
      })
      timeFormatting[1,] <- sapply(timeFormatting[1,], function(s) sub(".000","",s))
      timeFormatting[2,] <- paste(" (UTC",timeFormatting[2,], ")")
      timeFormatting <-  paste(timeFormatting[1,],timeFormatting[2,])
      
      if(any(names(x) == "relatedDischarges")) {
        
        if (is.null(x$relatedDischarges$value)) {
          discharge <-"N/A" 
        }
        else {
          discharge <- x$relatedDischarges$value
        }
        
        if (is.null(x$points$value)) {
          gageHeight <- "N/A"
        }
        else {
          gageHeight <- x$points$value
        }
      } else if (any(names(x) == "relatedGageHeights")) {
        if (is.null(x$relatedGageHeights$value)) {
          gageHeight <- "N/A"
        }
        else {
          gageHeight  <- x$relatedGageHeights$value
        }
        if (is.null(x$points$value)) {
          discharge <- "N/A"
        }
        else {
          discharge <- x$points$value  
        }
        
      } else {
        if (is.null(x$points$value)) {
          discharge <- "N/A"
        }
        else {
          discharge <- x$points$value  
        }
        gageHeight  <- "N/A"
      }
      
      data.frame(dateTime[,1], timeFormatting, discharge, gageHeight, stringsAsFactors = FALSE)
      
      
    })
    
    names(min.max) <- paste0("data$", names(data)[i], "$", names(subset))
    results <- append(results, min.max) 
    
  }
  
  
  maximums <- results[grep("max", names(results))]
  maximums_index <- c(grep("gageHeight", names(maximums)), 
                      grep("discharge", names(maximums)), 
                      grep("dailyDischarge", names(maximums)))
  maximums <- maximums[maximums_index]
  
  minimums <- results[grep("min", names(results))]
  minimums_index <- c(grep("gageHeight", names(minimums)), 
                      grep("discharge", names(minimums)), 
                      grep("dailyDischarge", names(minimums)))
  minimums <- minimums[minimums_index]
  
  results <- list()
  results <- append(results, maximums)
  results <- append(results, minimums)
  
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
    discharge=data$discharge$qualifiers, 
    gageHeight=data$gageHeight$qualifiers,
    dailyDischarge=data$dailyDischarge$qualifiers)
  
  return(sapply(data, function(x) {
    if(! is.null(x$qualifiers)) {
      x$max$points <- applyQualifiersToValues(x$max$points, x$qualifiers)
      x$min$points <- applyQualifiersToValues(x$min$points, x$qualifiers)
      x$max$relatedGageHeights <- applyQualifiersToValues(x$max$relatedGageHeights, consolidatedQualifiers$gageHeight)
      x$min$relatedGageHeights <- applyQualifiersToValues(x$min$relatedGageHeights, consolidatedQualifiers$gageHeight)
      x$max$relatedDischarges <- applyQualifiersToValues(x$max$relatedDischarges, consolidatedQualifiers$discharge)
      x$min$relatedDischarges <- applyQualifiersToValues(x$min$relatedDischarges, consolidatedQualifiers$discharge)
    }
    return(x)
  }))
}

applyQualifiersToValues <- function(points, qualifiers) {
  if(is.null(points)) return(points)
  
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
