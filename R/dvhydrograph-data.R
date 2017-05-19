#' Create vertical step edges between estimated and non-estimated series
#' @description Given a stat TS and an estimated TS this function creates
#' vertical lines connecting the steps between those TS.
#' @param stat the parsed non-estimated time series
#' @param est the parsed estimated time series
#' @param excludeZeroNegativeFlag whether or not zero / negative values
#' will be removed
#' @return a list of vertical lines connecting steps between stat and est
#' @importFrom dplyr arrange
#' @importFrom dplyr select
getEstimatedEdges <- function(stat, est, excludeZeroNegativeFlag=NULL){
  estEdges <- list()

  if(is.null(stat) || is.null(est) || isEmptyOrBlank(est[['points']][['value']]) || isEmptyOrBlank(stat[['points']][['value']])){
    return(NULL)
  }

  #Don't render edges for values that will be removed if we are exculding zero and negative values
  if(!is.null(excludeZeroNegativeFlag) && excludeZeroNegativeFlag){
    stat[['points']] <- removeZeroNegative(stat[['points']])
    est[['points']] <- removeZeroNegative(est[['points']])
  }

  est <- est[['points']][c('time', 'value')]
  stat <- stat[['points']][c('time', 'value')]

  . <- NULL # work around warnings from devtools::check()
  estData <- est %>% as.data.frame %>% mutate(set=rep('est', nrow(.)))
  statData <- stat %>% as.data.frame %>% mutate(set=rep('stat', nrow(.)))

  #Merge data into a single DF
  data <- rbind(estData, statData)
  
  # work around irrelevant warnings from devtools::check()
  time <- NULL
  y0 <- 0
  value <- 0
  set <- NULL
  lag <- NULL
  
  estEdges <- data %>% arrange(time) %>%
          mutate(y0 = ifelse(set != lag(set), lag(value), NA)) %>%
          filter(set != lag(set)) %>% dplyr::select(time, y0, y1 = value, newSet=set) %>% as.list
  
  return(estEdges)
}

#' Use the last point plus 1 day in seconds to extend step
#' the points do not have times, but the x limit is extended with a time to show the whole day
#' the step needs to be extended to meet this time
#' @param toPlot list of items that will be called in the do.call 
extendStep <- function(toPlot){
  #check first whether it's a feature added to the plot as a step
  isStep <- 'type' %in% names(toPlot) && toPlot[['type']] == "s"
  
  if(isStep){
    daySeconds <- 24 * 60 * 60 #1 day in seconds
    toPlot$x <- c(toPlot$x,  tail(toPlot$x, 1) + daySeconds)
    toPlot$y <- c(toPlot$y,  tail(toPlot$y,1))
    toPlot$legend.name <- c(toPlot$legend.name,  tail(toPlot$legend.name,1))
  }
  
  return(toPlot)
}

