

#'@export
getValue <- function(ts, param, ...){
  val <- ts$values[[param]]
  return(validParam(val, ...))
}

#'@export
getInput <- function(ts, param, ...){
  val <- ts$inputs[[param]]
  return(validParam(val, ...))
}

validParam <- function(val, required = FALSE){
  if (is.null(val)){
    if (required){
      stop('required value ', param, ' missing.')
    }
    return(" ")
  } else {
    return(val)
  }
}

#'@importFrom httr GET add_headers verbose content url_ok
#'@export
getJSON = function(url, auth){  
  
  response <- GET(url, 
                  config=list(ssl.verifypeer = FALSE), 
                  add_headers('Authorization' = auth, 
                              'Connection'='keep-alive', Accept='application/json'))
  
  url_ok(response$url)
  json <- content(response)
  return(json)
}