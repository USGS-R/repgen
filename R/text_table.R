#'@export
text_table <- function(){
  
  out <- '           Time          DISCHARGE       Gage Height    \n
 DATE           HH MM SS            (CFS)       (CMS)       (FT)       (M)  \n
  -------         -------      ------     ------         --------        -----'
  cat(out)
}