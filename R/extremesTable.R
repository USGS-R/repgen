#'@export
extremesTable <- function(ts){
  headers <- c('DATE', 'HH:MM:SS','(CFS)','(CMS)','(FT)', '(M)')
  ncol <- length(headers)
  dasher <- rep('-----',ncol)
  
  tbl <- data.frame(matrix(dasher, ncol = ncol),stringsAsFactors = FALSE, check.names = FALSE)
  names(tbl) <- headers
  fullVars <- c('maxXDischarge','minXDischarge', 'maxXStage','minXStage')
  retVars <- c('maxDailyDischarge','minDailyDischarge')
  for (i in 1:length(fullVars)){
    var <- fullVars[i]
    tbl <- rbind(tbl,c(getDate(ts,c(var,'Date')),getTime(ts,c(var,'Time'), tz=F), 
                       getNum(ts,c(var,'CFS')),getNum(ts,c(var,'CMS')), #failing to get minXStageDischargeCMS 
                       getNum(ts,c(var,'StageFt')),getNum(ts,c(var,'StageM'))))
  }
  
  for (i in 1:length(retVars)){
    var <- retVars[i]
    tbl <- rbind(tbl,c(getDate(ts,c(var,'Date')), " ",
                       getNum(ts,c(var,'CFS')),getNum(ts,c(var,'CMS')),
                       " ", " "))
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