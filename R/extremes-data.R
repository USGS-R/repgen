#'@title create a flat text 'extremes table' type output table
#'@param ts a timeseries list that comes from valid extremes json
#'@return string table
#'@export
extremesTable <- function(data){
  df <- data.frame(matrix(nrow=6, ncol=4))
  colnames(df) <- c("Date", "Time", "Discharge (cfs)", "Gage Height (ft)")

  row.names(df) <- c("Max inst GH and corresponding Q:", 
                     "Max inst Q and corresponding GH",
                     "Max daily Q",
                     "Min inst GH and corresponding Q:",
                     "Min inst Q and corresponding GH",
                     "Min daily Q")
  
  index <- which(names(data) %in% c("gageHeight", "discharge", "dailyDischarge")) 
  results <- list()
  
  for (i in index) {  
    
    min.max <- lapply(data[[i]], function(x) {
      
      dateTime <- unlist(strsplit(x$points$time[[1]], split="[T]"))

      date <- dateTime[1]
      
      date <- strftime(date,"%m-%d-%Y")
      
      time <- c(substr(dateTime[2], 1, 12), substr(dateTime[2], 13, 18))
      
      timeUTC <- paste0(time[1], " (UTC", time[2], ")")
      
      timeUTC <- sub(".000","",timeUTC)
      
      if(any(names(x) == "relatedDischarges")) {
        discharge <- x$relatedDischarges$value[1]
        gageHeight <- x$points$value[1]
      } else if (any(names(x) == "relatedGageHeights")) {
        gageHeight  <- x$relatedGageHeights$value
        discharge <- x$points$value
      } else {
        discharge <- x$points$value
        gageHeight  <- "N/A"
      }
      
      c(date, timeUTC, discharge, gageHeight)
      
      
    })
    
    names(min.max) <- paste0("data$", names(data)[i], "$", names(data[[i]]))
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



flattenParam <- function(param){
  baseParam <- strsplit(gsub("([A-Z])", " \\1", param[1]), " ")[[1]]
  param <- paste(unique(c(baseParam, param[-1])), collapse='')
  return(param)
}