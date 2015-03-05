library('jsonlite')
load_json <- function(file){
  json = fromJSON(file)
  return(json)
}