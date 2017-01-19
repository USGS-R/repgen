#'@title pad a table in mono text
#'@description create a simple text table
#'@param reportObject a data.frame with character data only. 
#'If \code{row.names} are present, they will be injected 
#'into the table output. 
#'@param align a character vector of length 1 or of equal length to the 
#'number of columns in \code{data}
#'@param space a numeric vector of length 1 or of equal length to the 
#'number of columns in \code{data}. Used to layout the table. Is nchar 
#'width of a single column
#'@return a character output for the table
#'@export
padTable <- function(reportObject, align = 'left', space = 16){
  table <- ""
  if (length(align) != 1 | length(space) != 1) 
    stop('multi length args for align or space are not yet supported')
  if (align != 'left') stop(align, ' not yet supported')
  
  buffer <- paste0("%-",space,'s') # negative sign for left align
  
  # for headers --
  baseChar <- sprintf(buffer, vector(mode = 'character', length=ncol(reportObject)))
  substring(baseChar, 1) <- names(reportObject)
  flatRow <- paste(baseChar, collapse='')
  table <- paste(table,flatRow,'\n', collapse = '')
  
  for (i in 1:nrow(reportObject)){
    baseChar <- sprintf(buffer, vector(mode = 'character', length=ncol(reportObject)))
    substring(baseChar, 1, last = space-1) <- as.character(reportObject[i,])
    flatRow <- paste(baseChar, collapse='')
    if (row.names(reportObject[i,]) != " "){
      table <- paste(table, row.names(reportObject[i,]),'\n', collapse = '')
    }
    table <- paste(table,flatRow,'\n', collapse = '')
  }
  
  return(table)
}