#' Used by UV Hydrograph report to organize and fit plots and tables in report
#' 
#' @description sets margins and lays out report in an attractive way for UV Hydrograph
#' 
#' @param feature the plot or table to include
#' @param isTable TRUE or FALSE if the feature is a table. defaults to FALSE
#' @param m the month name for the feature
#' @param mar_values the margin values for the feature to ensure space for plot and legend
#' 
#' @return the formatted feature requested
#' 
printReportFeature <- function(feature, isTable=FALSE, m=NULL, mar_values=c(8, 3, 4, 3)){
  if(!is.null(mar_values)){
    par(mar=mar_values)
  }
  
  if(!isEmpty(feature)){
    if(isTable){
      print(kable(feature))
      cat("\n\n")
    } else if(!is.null(m)){
      msg <- paste(feature, 'in', m)
      cat(msg)
    } else if(!is.null(feature$side.6)) {
      printWithThirdYAxis(feature)
      cat("\n\n")
    } else {
      print(feature)
      cat("\n\n")
    }
  } else {
    return()
  }
}

#' Takes comments formatted with special escape characters and converts them to HTML breaks
#' and includes a break at the end between comments
#' 
#' @description Formats comments more effectively for including them in HTML table
#' 
#' @param comments which have \\r\\n indicating line breaks
#' 
#' @return comments formatted with html breaks instead of escape pattern
#' 
formatComments <- function(comments){
  split_comments <- unlist(comments)
  if(isEmptyOrBlank(split_comments)){return(split_comments)}
  htmlbreaks_inside <- lapply(split_comments, gsub, pattern="\r\n", replacement="<br/>")
  htmlbreaks_end <- lapply(htmlbreaks_inside, paste0, "<br/>", collapse="")
  table_comments <- do.call(paste0, htmlbreaks_end)
  return(table_comments)
}

#' Inserts the SIMS url (if it exists) into the base of the report
#' 
#' @description takes the SIMS url and formats it 
#' for including in the report as a link
#' 
#' @param simsUrl The simsUrl requested to turn into a link
#' 
#' @return the HTML link for SIMS url
#' 
getSimsUrl<- function(simsUrl){
  if(isEmptyOrBlank(simsUrl)) {
    simsLink <- "SIMS URL: NA"
  } else {
    simsLink <- paste("<a href='",simsUrl,"' target='_blank'>","SIMS URL:",simsUrl,"</a>")
  }
  return(simsLink)
}

#'Put the waterdata.usgs.gov url (if it exists) into the base of the report
#'
#'@description takes the waterdata url and formats it for including it
#'in the report as a link
#'
#'@param waterdataUrl The waterdata url requested to turn into a link
#'
#'@return The HTML link for waterdata url
#'
getWaterDataUrl <- function(waterdataUrl) {
  if (isEmptyOrBlank(waterdataUrl)) {
    waterdataLink <- "waterdata.usgs.gov URL: NA"
  } else {
    waterdataLink <- paste("<a href='",waterdataUrl,"' target='_blank'>","waterdata.usgs.gov URL:",waterdataUrl,"</a>")
  }
  return(waterdataLink)
}

#' Convert the string to the equivalent HTML code
#' 
#' @param characters The string to convert
#' 
#' @return The equivalent HTML codes for that string
#' 
convertStringToTableDisplay <- function(characters){
  characters <- gsub(">", "&gt;", gsub("<", "&lt;", characters))
  return(characters)
}

#' Convert the String from HTML code to the equivalent raw characters
#' 
#' @param characters The characters to convert
#' 
#' @return The equivalent string for the HTML codes
#' 
convertTableDisplayToString <- function(characters){
  characters <- gsub("&gt;", ">", gsub("&lt;", "<", characters))
  return(characters)
}

#' shared logo used for reports
#' 
#' @description provides rmarkdown with the image for USGS logo
#'  
#' @return logo to report
getLogo <- function(){
  jpg_filepath <- 'usgs_logo.jpg'
  markdown_text <- noquote(paste0("![](", jpg_filepath, ")"))
  return(markdown_text)
}

#' if a value is null, returns an empty string instead of null, and
#' if a value is present, the value is returned
#' 
#' @description makes sure that the slot in the data frame is not missing by
#' exchanging null values as empty character or the original value if not null
#' also works on list objects
#' 
#' @param val the value or values you want to check for null and mask
#' 
#' @return either the original value or a null empty object
#' 
nullMask <- function(val) {
  val <- unlist(val)
  if(!isEmptyOrBlank(val)) {
    result <- val
  } else {
    result <- ""
  }
  return(result)
}

#' @title timeFormatting
#' @description Formats date to passed-in format mask, and time to "(UTC [offset] )"
#' @param timeVals String with format of "YYYY-MM-DDTHH:MM:SS.SSS-UTC offset".
#' @param dateFormatMask String with preferred output date format
#' @return list with date in first position, time in second position.
timeFormatting <- function(timeVals, dateFormatMask){
  if(!isEmptyOrBlank(timeVals)) {
    dateTime <- (strsplit(timeVals, split="[T]"))
    dateFormat <- strftime(dateTime[[1]][1], dateFormatMask)
    
    #Break apart, format dates/times, put back together.
    timeFormatting <- sapply(dateTime[[1]][2], function(s) {
      #Break apart the date and time into a list of two strings
      m <- regexec("([^-+]+)([+-].*)", s)
      splitTime <- unlist(regmatches(s, m))[2:3]
      return(splitTime)
    })
    timeFormatting[[1]] <- sapply(timeFormatting[[1]], function(s) sub(".000","",s))
    timeFormatting[[2]] <- paste0(" (UTC ",timeFormatting[[2]], ")")
    timeFormatting <-  paste(timeFormatting[[1]],timeFormatting[[2]])
  } else {
    dateFormat <- ""
    timeFormatting <- ""
  }
  return(list(date = dateFormat, time = timeFormatting))
}

#' Returns a list of comments or an empty character if there are no comments
#' 
#' @description Accepts the comments string and checks to see if it's null or empty,
#' and if it is, returns an empty string
#' 
#' @param comments The text comments from the JSON data
#' 
#' @return comments as they were passed or an empty string if empty or null

getComments <- function(comments) {
  comm <- unlist(comments)
  if (!isEmptyOrBlank(comm)) {
    value <- comm
    
  } else {
    value <- ""
  }
  return(value)
}

#' Create Flat Text, "qualifiers table" Type Output Table
#' 
#' @param inQualifiers data frame of filtered (for SVP) or all (for SRS) qualifiers.
#' @return list of deduplicated qualifiers with column names.
formatQualifiersTable <- function(inQualifiers) {
    if (isEmptyOrBlank(inQualifiers) || nrow(inQualifiers) == 0) return()
  
        columnNames <- c("Code",
                         "Identifier",
                         "Description")
  
        toRet <- inQualifiers[!duplicated(inQualifiers), ]
  colnames(toRet) <- columnNames

  return(toRet)
}

#' Create a comma-delimited string of qualifier codes
#' 
#' @param inQualifiers data frame of filtered (for SVP) or all (for SRS) qualifiers.
#' @return comma-delimited string of qualifier codes
formatQualifiersStringList <- function(inQualifiers) {
  if(length(inQualifiers[[1]]) < 1) return("");

  builtQualifiers <- ""
  if(nrow(inQualifiers) > 0) {
    for(i in 1:nrow(inQualifiers)) {
      #Due to HTML hack being used for comments on SRS reports can't use kable to render table and thus need to use a hack to show greaterthan and other special HTML codes
      #Same method is used here for consistency since both reports use HTML tables formatted in the same way
      builtQualifiers <- paste0(builtQualifiers, convertStringToTableDisplay(inQualifiers[i,]$code), ",")
    }
    strLength <- nchar(builtQualifiers)
    if(strLength > 0) {
      builtQualifiers <- substr(builtQualifiers, 1, strLength-1)
    }
  }
  
  return(builtQualifiers)
}

#' Create a note on report about corrected value
#' 
#' @param diffData list of peak differences
#' @return boolean of where peak differences are >0.05
containsOutsideUncertainty <- function(diffData) {
  diff_list <- as.list(c(diffData))
  return(length(diff_list[grepl("\\*\\*", diff_list)]) > 0)
}

#' Return a list of columns for the Site Visit Peak report
#' 
#' @param includeComments boolean value about whether to include comments or not
#' @return list of columns
getSVPColumns <- function(includeComments)
  if(includeComments){
    columnNames <- c("Date",
                     "Time",
                     "Party",
                     "Sublocation",
                     "Verification Method",
                     "Reading",
                     "Uncertainty",
                     "Estimated Date",
                     "Estimated Time",
                     "Verification Comments",
                     "Corrected Value",
                     "Qualifier",
                     "Date",
                     "Time",
                     "Difference from Peak Verification Reading")
  } else {
    columnNames <- c("Date",
                     "Time",
                     "Party",
                     "Sublocation",
                     "Verification Method",
                     "Reading",
                     "Uncertainty",
                     "Estimated Date",
                     "Estimated Time",
                     "Corrected Value",
                     "Qualifier",
                     "Date",
                     "Time",
                     "Difference from Peak Verification Reading")
    return(columnNames)
  }