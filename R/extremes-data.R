#'@title create a flat text 'extremes table' type output table
#'@param rawData extremes report json string
#'@importFrom dplyr mutate
#'@return string table
#'@export
extremesTable <- function(rawData){
  data <- applyQualifiers(rawData)
  
  df <- data.frame(matrix(nrow=6, ncol=4))
  colnames(df) <- c("Date", "Time", "Discharge (cfs)", "Gage Height (ft)")

  row.names(df) <- c("Max inst GH and corresponding Q", 
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
      
      dateTime <- unlist(strsplit(x$points$time[[1]], split="[T]"))

      date <- dateTime[1]
      
      date <- strftime(date,"%m-%d-%Y")
      
      time <- c(substr(dateTime[2], 1, 12), substr(dateTime[2], 13, 18))
      
      timeUTC <- paste0(time[1], " (UTC", time[2], ")")
      
      timeUTC <- sub(".000","",timeUTC)
      
      if(any(names(x) == "relatedDischarges")) {
        
        if (is.null(x$relatedDischarges$value)) {
          discharge <-"N/A" 
        }
        else {
          discharge <- x$relatedDischarges$value[1]
        }
        
        if (is.null(x$points$value)) {
          gageHeight <- "N/A"
        }
        else {
          gageHeight <- x$points$value[1]
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
      
      c(date, timeUTC, discharge, gageHeight)
      
      
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
  
  for (n in 1:nrow(df)) {
    df[n,] <- results[[n]]
  }
  return(df)
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
