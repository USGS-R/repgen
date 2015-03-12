#'@importFrom httr POST
#'@export 
authenticateUser <- function(username, password){
  
  
  if(missing(username)){
    stop('username required for authentication')
  }
  
  if(!interactive() & missing(password)){
    stop('No password supplied to authenticate_sciencebase in a non-interactive session.')
  }else{
    password = ifelse(missing(password), readPassword('Please enter your Active Directory password:'), password)
  }
  
  
    #https://cida-eros-authdev.er.usgs.gov:8443/auth-webservice/auth/ad/token/
  ## authenticate
  resp = POST('https://cida-test.er.usgs.gov/auth-webservice/auth/ad/token/', accept_json(),
              body = list(username=username, password=password), encode='form', config=list(ssl.verifypeer = FALSE))
  return(content(resp)$tokenId)
}

readPassword <- function(prompt) {
  if (exists(".rs.askForPassword")) {
    pass <- .rs.askForPassword(prompt)
  } else {
    pass <- readline(prompt)
  }
  return (pass)
}

