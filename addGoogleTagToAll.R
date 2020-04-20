webDirectory <- "/Users/npm4/GitHub/lipdverse/html"
direc <- list.dirs(webDirectory)
direc <- direc[-1]#omit main directory
tag <- readLines(file.path(webDirectory,"gatag.html"))
library(purrr)
library(tidyverse)


addTagToDirec <- function(td){
  af <- list.files(td,full.names = TRUE,pattern = ".html")
  if(length(af)>1){
    res <- purrr::map(af,addGoogleTracker,tag)
  }
  return(td)
}

for(i in 1:length(direc)){
  print(i)
  addTagToDirec(direc[i])
}
