#make sure that all lipdverse files have their most up to date versions in the data pages
syncDataPages <- function(){
D <- loadLipdverseDatabase()
webDataPath <- "~/Dropbox/lipdverse/html/data/"

allDataDsids <- list.dirs(webDataPath,recursive = FALSE,full.names = FALSE)

allDsids <- map_chr(D,"datasetId")

#are there any dsids missing entirely?
missingDsids <- setdiff(allDsids,allDataDsids)

if(length(missingDsids) > 0){
wm <- which(missingDsids %in% allDsids)
M <- D[wm]
#if so, create those pages
walk(M,createDataWebPage,.progress = TRUE)
}

#now check the most recent versions in the data and the folders

dataMostRecentVersion <- map_chr(D,getVersion) %>% as.numeric_version()


getMostRecentWebVersion <- function(dsid){
  av <- list.dirs(file.path(webDataPath,dsid),recursive = FALSE,full.names = FALSE) %>%
    stringr::str_replace_all("_","\\.") %>%
    as.numeric_version() %>%
    max() %>%
    as.character()

  return(av)
}

webMostRecentVersion <- map_chr(allDsids,getMostRecentWebVersion,.progress = TRUE) %>% as.numeric_version()

test <- unlist(webMostRecentVersion)

}
