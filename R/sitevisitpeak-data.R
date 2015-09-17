#'@title create a flat text 'sitevisitpeak table' type output table
#'@param rawData sitevisitpeak report json string
#'@importFrom dplyr mutate
#'@return data.frame table

formatSVPData <- function(data, columnNames){
  toRet = data.frame(stringsAsFactors = FALSE)
  for(listElements in list(data)){
    
    dateTime <- (strsplit(listElements$time, split="[T]"))
    date <- strftime(dateTime[[1]][1], "%m/%d/%Y")
    
    #Break apart, format dates/times, put back together.
    timeFormatting <- sapply(dateTime[[1]][2], function(s) strsplit(s,split="[-]")[[1]])
    timeFormatting[[1]] <- sapply(timeFormatting[[1]], function(s) sub(".000","",s))
    timeFormatting[[2]] <- paste(" (UTC",timeFormatting[[2]], ")")
    timeFormatting <-  paste(timeFormatting[[1]],timeFormatting[[2]])
    toAdd = c(listElements$visitStatus,
              date,
              timeFormatting,
              listElements$party, 
              listElements$monitoringMethod, 
              listElements$value,
              listElements$uncertainty, 
              "", #Need to add the estimated Date?
              listElements$comments,
              listElements$correctedValue,
              listElements$qualifier,
              listElements$ivDate,
              listElements$ivTime,
              listElements$ivDifference)
    
    toRet <- rbind(toRet, data.frame(t(toAdd),stringsAsFactors = FALSE))
  }
  colnames(toRet) <- columnNames
  rownames(toRet) <- NULL
  
  return(toRet)
}

#Change applyQualifiers to change the data to add the qualifiers to the data itself.
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
