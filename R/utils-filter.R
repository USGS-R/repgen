#' Remove zero or negative values
#' 
#' @description If negative or zero values are passed into function, turn them into NA 
#' so we can log the plot
#' 
#' @param df any data frame that it will check for negative or zeroes
#' @param column (Optional) [DEFAULT: 'value'] The column to look for zero/negative values in
#' 
#' @importFrom dplyr filter
removeZeroNegative <- function(df, column='value'){
  df <- df
  df <- filter(df, df[[column]] > 0)
  return(df)
}

#' Extract time value points list by a single month
#' 
#' @description Subset the data passed into the function by the month passed in
#' 
#' @param pts a list of time and value points
#' @param onlyMonth the numeric month to subset the list of time and value points by
#' 
subsetByMonth <- function(pts, onlyMonth) {
  if(!is.null(pts) && nrow(pts) > 0) {
    return(subset(pts, month == onlyMonth))
  }
  return(pts)
}

#' Merge two lists
#' 
#' @description Merges the two lists passed in. The lists must have the exact same structure.
#' 
#' @param list1 The first list to merge
#' @param list2 The second list to merge
mergeLists <- function(list1, list2){
  returnList <- list()
  
  if(!isEmptyOrBlank(list1) && !isEmptyOrBlank(list2)){
    returnList <- mapply(c, list1, list2, SIMPLIFY = FALSE)
  } else if(!isEmptyOrBlank(list1)){
    returnList <- list1
  } else if(!isEmptyOrBlank(list2)){
    returnList <- list2
  }
  
  return(returnList)
}

#' Attach Full Data to Sub Frame
#' 
#' @description Given a data frame with some sub-data frame will take the contents of the row of the
#' main frame and create a new row for each row of the sub data frame with the main data attached.
#' Example: 
#' 
#'    x  y  sub-frame
#' 1  1  2  [1 z=3, 2 z=4]
#' 2  3  4  [1 z=5, 2 z=6]
#' -----------------------
#'    x  y  z
#' 1  1  2  3
#' 2  1  2  4
#' 3  3  4  5
#' 4  3  4  6
#' 
#' @param mainFrame The primary data frame to work on
#' @param subFrameIdentifier the name of the column containing the sub-frame
attachFullDataToSubFrame <- function(mainFrame, subFrameIdentifier){
  newData <- data.frame()
  
  if(!isEmptyOrBlank(mainFrame)){
    for(i in 1:nrow(mainFrame)){
      attachData <- mainFrame[i,][-which(names(mainFrame[i,]) == subFrameIdentifier)]
      
      for(j in 1:nrow(mainFrame[i,][[subFrameIdentifier]][[1]])){
        newRow <- as.data.frame(c(mainFrame[i,][[subFrameIdentifier]][[1]][j,], attachData), stringsAsFactors=FALSE)
        
        if(isEmptyOrBlank(newData)){
          newData <- newRow
        } else {
          newData <- merge(newRow, newData, all=TRUE)
        }
      }
    }
  }
  
  return(newData)
}