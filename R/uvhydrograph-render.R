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



########################## new functions
plotting <- function(object, sublist) {
  if (!is.null(sublist[['y']]) && length(sublist[['y']]) > 0){
    if (sublist[['type']]=='l'){
      object <- do.call(lines, append(sublist, list(object=object))) 
    } else if (sublist[['type']]=='p'){
      object <- do.call(points, append(sublist, list(object=object)))  
    }
  }
  return(object)
}  
