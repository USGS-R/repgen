############ functions:

subsetByMonth <- function(pts, onlyMonth) {
  if(!is.null(pts) && nrow(pts) > 0) {
    return(subset(pts, month == onlyMonth))
  }
  return(pts)
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