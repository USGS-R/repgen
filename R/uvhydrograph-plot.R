#'@export
uvhydrographPlotMonth <- function(data,xlim=c(NA,NA)){ 
  allUVdata <- getAllUVdata(data)

  #gets back a list of all the chunks of data from json, names appropriately
  
  allUVdataStyled <- styleUVdata(allUVdata)
  #just lines and points
  
  orderData() #or you could handle this in getAllUVdata()
  
  gsplottify () #takes all the data and styles and makes it into a gsplot call
  
  #add axes and grid
  
  print(gsplot)
}

secondaryUvPlot <- function(data){
  
  
}

