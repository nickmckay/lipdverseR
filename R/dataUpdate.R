#code to update the lipdverse data holdings

#get all dsids, dsns, and most recent versions

getMeta <- function(L){
  dsid <- L$datasetId
  dsn <- L$dataSetName
  vers <- sapply(L$changelog,"[[","version") %>% as.numeric_version() %>% max() %>% as.character()
  df <- data.frame(dsid = dsid, dsn = dsn, version = vers)
  return(df)
}


updateDataWebpages <- function(D,webdir = "/Volumes/data/Dropbox/lipdverse/html"){

dsMeta <- purrr::map_dfr(D,getMeta)

if(length(unique(dsMeta$dsid)) != nrow(dsMeta)){
  stop("Number of unique datasetIds is not what it should be")
}

oldDsMeta <- read_csv(file = file.path(webdir,"data","dsid-inventory.csv"))


j <- dplyr::full_join(oldDsMeta,dsMeta,by = c("dsid")) %>%
  filter(version.x != version.y | is.na(version.x) | is.na(version.y))

webToCreate <- j %>%
  filter(version.x != version.y | is.na(version.x))

if(nrow(webToCreate) > 0){
dsnsToCreate <- c(webToCreate$dsn.y)
D2C <- D[dsnsToCreate]
dum <- purrr::map(D2C,createDataWebPage,webdir = webdir)
#update inventory
write_csv(dsMeta,file = file.path(webdir,"data","dsid-inventory.csv"))
}

#look for removed

removedDataset <- j %>%
  filter(is.na(version.y))

if(nrow(removedDataset)>0){
  warning("Datasets appear to have been removed. I didn't delete anything though")
return(removedDataset)
}


}

