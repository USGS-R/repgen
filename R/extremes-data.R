#'@title create a flat text 'extremes table' type output table
#'@param ts a timeseries list that comes from valid extremes json
#'@return string table
#'@export
extremesTable <- function(ts){
  headers <- c('DATE', 'HH:MM:SS','(CFS)','(CMS)','(FT)', '(M)')
  ncol <- length(headers)
  dasher <- rep('-----',ncol)
  tbl <- data.frame(matrix(dasher, ncol = ncol), stringsAsFactors = FALSE, check.names = FALSE)
  names(tbl) <- headers
  fullVars <- c('maxXDischarge','minXDischarge', 'maxXStage','minXStage', 'maxDailyDischarge','minDailyDischarge')
  for (i in 1:length(fullVars)){
    var <- fullVars[i]
    
    tbl <- rbind(tbl,c(getReportMetadata(ts,flattenParam(c(var,'Date'))),getReportMetadata(ts,flattenParam(c(var,'Time'))),
                       getReportMetadata(ts,flattenParam(c(var,'Discharge','CFS'))),getReportMetadata(ts,flattenParam(c(var,'Discharge','CMS'))),
                       getReportMetadata(ts,flattenParam(c(var,'Stage','Ft'))),getReportMetadata(ts,flattenParam(c(var,'Stage','M')))))
  }
  
  
  # to do: should use key with param names
  rwNames <- c(" ",
               'MAX INST DISCHARGE AND CORRESPONDING Gage height',
               'MIN INST DISCHARGE AND CORRESPONDING Gage height',
               'MAX INST Gage height AND CORRESPONDING DISCHARGE',
               'MIN INST Gage height AND CORRESPONDING DISCHARGE',
               'MAX DAILY DISCHARGE',
               'MIN DAILY DISCHARGE')
  
  row.names(tbl) = rwNames
  return(tbl)
}

flattenParam <- function(param){
  baseParam <- strsplit(gsub("([A-Z])", " \\1", param[1]), " ")[[1]]
  param <- paste(unique(c(baseParam, param[-1])), collapse='')
  return(param)
}