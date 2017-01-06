#' Get JSON to list from an authenticated endpoint.
#' 
#' @param url The JSON endpoint.
#' @param ... Additional parameters passed to \code{checkAuth}.
#' @param verbose Send output to screen during GET request when TRUE; do not send
#'        output to screen during GET request otherwise.
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr verbose
#' @importFrom httr content
#' @importFrom httr http_status
#' @keywords internal
getJSON = function(url, ..., verbose = FALSE) {  
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
