#'@title get JSON to list from an authenticated endpoint
#'@param url the json endpoint
#'@param ... additional parameters passed to checkAuth
#'@param verbose output to screen during GET
#'@importFrom httr GET add_headers verbose content http_status
#'@keywords internal
getJSON = function(url, ..., verbose = FALSE){  
  checkAuth(...)
  
  response <- GET(url, 
                  config=list('ssl.verifypeer' = FALSE, 'verbose' = verbose), 
                  add_headers('Authorization' = getAuth(), 
                              'Connection'='keep-alive', Accept='application/json'))
  
  if (!http_status(response)$category == "success"){
    stop("GET failed on ", url)
  }
  json <- content(response, as = "parsed")
  return(json)
}

getLogo <- function(){
  jpg_filepath <- 'usgs_logo.jpg'
  markdown_text <- noquote(paste0("![](", jpg_filepath, ")"))
  return(markdown_text)
}
