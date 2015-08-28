#
# Starting point, creates RMD and runs rendering
#
startVDiagramRender <- function(data, output, author) {
  output_dir <- getwd()
  rmd_file <- makeVDiagramRmd(system.file('vdiagram', package = 'repgen'), data, output, output_dir)
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir, intermediates_dir=output_dir)
  return(out_file)
}

makeVDiagramRmd <- function(rmd_dir, data, output, wd){
  rmdName <- 'vdiagram.Rmd'
  rmd_file <- file.path(rmd_dir, rmdName)
  
  newPage = ifelse(output == "pdf", '$\\pagebreak$', '------')
  tempRmd <- tempfile(pattern = 'vdiagram', fileext = '.Rmd', tmpdir = wd)
  
  con <- file(rmd_file)
  rawText <- readLines(con)
  close(con)
  replacePlot <- "renderVDiagram(data)"
  replaceTable <- "vdiagramTable(data, output)"
  
  nPages <- length(data$pages)
  metaData <- vector(mode = 'list', length = nPages) #lol
  # creates multi Rmd pages for output, truncates plots and tables. Returns metaData list globally, which would be nice to avoid
  # should probably break this up into two calls. One that returns Rmd handle, the other w/ data
  for (i in 1:nPages){
    pageName <- names(data$pages)[i]
    pageData <- data$pages[[pageName]]
    metaData[[i]] <- pageData
    pageText <- rawText
    pageText[pageText == replacePlot] <- sprintf('renderVDiagram(metaData[[%s]])', i)
    pageText[pageText == replaceTable] <- sprintf('vdiagramTable(metaData[[%s]], output)', i)
    cat(c(pageText,newPage), file = tempRmd, sep = '\n', append = TRUE)
  }
  metaData <<- metaData
  return(tempRmd)
}

#
# Called from VDiagram RMD files
#
renderVDiagram <- function(data){
  if (!is.null(data$pages)){
    for (i in 1:length(names(data$pages))){
      pageName <- names(data$pages)[i]
      createVdiagram(data$pages[[pageName]])
    }
  } else {
    createVdiagram(data)
  }
}

createVdiagram <- function(data) {
  data <- parseVDiagramData(data)
  vplot(data)
}