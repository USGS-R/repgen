#'@title create a flat text 'extremes table' type output table
#'@param ts a timeseries list that comes from valid extremes json
#'@return string table
#'@export
extremesTable <- function(data){

  index <- which(names(data) %in% c("gageHeight", "discharge", "dailyDischarge")) 
  results <- list()
  
  for (i in index) {  
    
    subset <- data[[i]][which(names(data[[i]])%in%c("min","max"))]
    
    min.max <- lapply(subset, function(x) {

      dateTime <- t(data.frame(strsplit(x$points$time, split="[T]")))
      dateTime[,1] <- strftime(dateTime[,1], "%m-%d-%Y")
      
      timeFormatting <- sapply(dateTime[,2], function(s) strsplit(s,split="[-]")[[1]])
      timeFormatting[1,] <- sapply(timeFormatting[1,], function(s) sub(".000","",s))
      timeFormatting <- mapply(function(s,d) paste(s," (UTC",d,")"), timeFormatting[1,], timeFormatting[2])
      
      if(any(names(x) == "relatedDischarges")) {
        
        if (is.null(x$relatedDischarges$value)) {
          discharge <-"N/A" 
        }
        else {
          discharge <- x$relatedDischarges$value
        }
        
        if (is.null(x$points$value)) {
          gageHeight <- "N/A"
        }
        else {
          gageHeight <- x$points$value
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
      
      data.frame(dateTime[,1], timeFormatting, discharge, gageHeight)
      
      
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
  
  #Next three rows are probably unnecessary. Instead, just change the column names and add
  # a header name for each table.
  #Currently have a list of the dataFrames. Only need to change the row/column names and done.
  
  colnames(df) <- c("Date", "Time", "Discharge (cfs)", "Gage Height (ft)")
  row.names(df) <- c("Max inst GH and corresponding Q",
                     rep("",tmp[[1]]-1),
                     "Max inst Q and corresponding GH",
                     rep("",tmp[[2]]-1),
                     "Max daily Q",
                     rep("",tmp[[3]]-1),
                     "Min inst GH and corresponding Q",
                     rep("",tmp[[4]]-1),
                     "Min inst Q and corresponding GH",
                     rep("",tmp[[5]]-1),
                     "Min daily Q")
  
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
