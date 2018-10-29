#' Create a Flat Text, "sitevisitpeak table" Type Output Table
#' 
#' @param readingsData data frame of parsed field visit readings data.
#' @param excludeComments boolean from report metadata of whether comments are included.
#' @param timezone timezone for data from report metadata
#' @return data frame of site visit peak data
sitevisitpeakTable <- function(readingsData, excludeComments, timezone){
  if(isEmptyOrBlank(readingsData)){
    return("The dataset requested is empty.")
  }
  
  dateFormat <- "%m/%d/%Y"
  toRet = data.frame(stringsAsFactors = FALSE)
  includeComments <- isNullOrFalse(excludeComments)
  columnNames <- getSVPColumns(includeComments)
  
  for(listRows in row.names(readingsData)){
    listElements <- readingsData[listRows,]
    fvTime <- flexibleTimeParse(listElements$visitTime, timezone=timezone, FALSE, TRUE)
    
    fvTimeFormatting <- timeFormatting(fvTime, dateFormat)
    estTimeFormatting <- timeFormatting(flexibleTimeParse(listElements$time, timezone=timezone, FALSE, TRUE), dateFormat)
    ivTimeFormatting <- timeFormatting(flexibleTimeParse(listElements$associatedIvTime, timezone=timezone, FALSE, TRUE), dateFormat)
    
    quals <- formatQualifiersStringList(listElements$qualifiers[[1]])
    
    diff <- readIvDifference(listElements$value, listElements$associatedIvValue)
    
    if(includeComments){
      toAdd = c(fvTimeFormatting$date,
                fvTimeFormatting$time,
                listElements$party, 
                listElements$sublocation, 
                listElements$monitoringMethod, 
                listElements$value,
                listElements$uncertainty,
                estTimeFormatting$date,
                estTimeFormatting$time,
                formatComments(getComments((listElements$comments))),
                listElements$associatedIvValue,
                quals,
                ivTimeFormatting$date,
                ivTimeFormatting$time,
                listElements$diffPeak)
    } else {
      toAdd = c(fvTimeFormatting$date,
                fvTimeFormatting$time,
                listElements$party, 
                listElements$sublocation, 
                listElements$monitoringMethod, 
                listElements$value,
                listElements$uncertainty,
                estTimeFormatting$date,
                estTimeFormatting$time,
                listElements$associatedIvValue,
                quals,
                ivTimeFormatting$date,
                ivTimeFormatting$time,
                diff)
      
    }
    
    toRet <- rbind(toRet, data.frame(t(toAdd),stringsAsFactors = FALSE))
  }
  colnames(toRet) <- columnNames
  rownames(toRet) <- NULL
  
  return(toRet)
}