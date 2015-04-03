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
    
    tbl <- rbind(tbl,c(getValue(ts,flattenParam(c(var,'Date'))),getValue(ts,flattenParam(c(var,'Time'))),
                       getValue(ts,flattenParam(c(var,'Discharge','CFS'))),getValue(ts,flattenParam(c(var,'Discharge','CMS'))),
                       getValue(ts,flattenParam(c(var,'Stage','Ft'))),getValue(ts,flattenParam(c(var,'Stage','M')))))
  }
  
  
  # to do: should use key with param names
  rwNames <- c(" ",
               'MAXIMUM INSTANTANEOUS DISCHARGE AND CORRESPONDING Gage height',
               'MINIMUM INSTANTANEOUS DISCHARGE AND CORRESPONDING Gage height',
               'MAXIMUM INSTANTANEOUS Gage height AND CORRESPONDING DISCHARGE',
               'MINIMUM INSTANTANEOUS Gage height AND CORRESPONDING DISCHARGE',
               'MAXIMUM DAILY DISCHARGE',
               'MINIMUM DAILY DISCHARGE')
  
  row.names(tbl) = rwNames
  return(tbl)
}

flattenParam <- function(param){
  baseParam <- strsplit(gsub("([A-Z])", " \\1", param[1]), " ")[[1]]
  param <- paste(unique(c(baseParam, param[-1])), collapse='')
  return(param)
}