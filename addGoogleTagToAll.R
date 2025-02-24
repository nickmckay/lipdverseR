webDirectory <- "~/Dropbox/lipdverse/html/data/"
direc <- list.dirs(webDirectory,recursive = TRUE,full.names = TRUE)
direc <- direc[-1]#omit main directory
tag <- readLines(file.path("~/Dropbox/lipdverse/html","gatag.html"))
library(purrr)
library(tidyverse)


addTagToDirec <- function(td){
  af <- list.files(td,full.names = TRUE,pattern = "sidebar.html")
  if(length(af)>0){
    res <- purrr::map(af,addGoogleTracker,tag)
  }
  return(td)
}

for(i in 1:length(direc)){
  if(i %% 1000 == 0){
  print(i)
  }
  addTagToDirec(direc[i])
}
