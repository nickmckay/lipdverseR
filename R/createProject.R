#' Create dahsboards for a project
#'
#' @param D list of lipd files
#' @import flexdashboard
#' @import maptools
#' @import readr
#' @import stringr
#' @import dplyr
#' @import magrittr
#' @import leaflet
#' @import lipdR
#' @import geoChronR
#' @import dygraphs
#' @import here
#' @param TS lipd timeseries object
#' @param webDirectory website directory
#' @param project project name
#' @param version project version
#' @param currentVersion (TRUE or FALSE) copy to to currentVersion folder?
#'
#' @return
#' @export
#'
#' @examples
createProjectDashboards <- function(D,TS,webDirectory,project,version,currentVersion = TRUE){
#if there's no html directory, create one
if(!dir.exists(file.path(webDirectory,project))){
  dir.create(file.path(webDirectory,project))
}

  if(!dir.exists(file.path(webDirectory,project,version))){
    dir.create(file.path(webDirectory,project,version))
  }

# setwd(lipdDir)
# zip(file.path(webDirectory,project,str_c(project,".zip")),files = list.files(lipdDir,pattern= "*.lpd"))
#
# D <- readLipd(lipdDir)
# TS <- extractTs(D)



save(list = c("D","TS"),file = file.path(webDirectory,project,version,str_c(project,version,".RData")))
save(list = c("D","TS"),file = file.path(webDirectory,"temp.RData"))

#remove columns we don't want to plot
varNames <- sapply(TS, "[[","paleoData_variableName")

#good <- which(!(varNames %in% c("year","depth","age")))
#TS <- TS[good]


#All datasets
dsn <- geoChronR::pullTsVariable(TS,"dataSetName")
ui <- which(!duplicated(dsn))
udsn <- dsn[ui]
lat <- geoChronR::pullTsVariable(TS,"geo_latitude")[ui]
lon <- geoChronR::pullTsVariable(TS,"geo_longitude")[ui]
elev <- geoChronR::pullTsVariable(TS,"geo_elevation")[ui]

archiveType <- geoChronR::pullTsVariable(TS,"archiveType")[ui]
link <- paste0(udsn,".html") %>%
  str_replace_all("'","_")


if(is.list(elev)){
  ge <- which(!sapply(elev,is.null))
  ne <- rep(NA,length(elev))
  ne[ge] <- as.numeric(unlist(elev))
  elev <- ge
}

#Organize metadata for map
map.meta <- data.frame(dataSetName = udsn, #datasetname
                       lat = lat,#lat
                       lon = lon,#lon
                       # elev = elev,#elevation
                       archiveType = factor(archiveType),#archiveType
                       link = link)#Link

#make project Rmd
createProjectRmd(webDirectory,project, version )

rmarkdown::render(file.path(webDirectory,project,version,"index.Rmd"))

failed = c()
for(i in 1:nrow(map.meta)){
  print(i)
  fname <- str_replace_all(udsn[i],"'","_")
 # if(!file.exists(file.path(webDirectory,project,version,str_c(fname,".html")))){

    thisTS <- TS[which(udsn[i] == dsn)]

    #look for chronTS
    chronTS <- try(extractTs(D[[udsn[i]]],whichtables = "meas",mode = "chron"))
    if(grepl(class(chronTS),"try-error") | length(chronTS)==0){
      chronTS <- NA
    }
    save(chronTS,file = file.path(webDirectory,"chronTemp.RData"))
    test = try(createDashboardRmd(thisTS = thisTS,i = i,webDirectory = webDirectory,project = project,version = version,chronTS = chronTS,map.meta = map.meta))
    Sys.sleep(1)
    test = try(rmarkdown::render(file.path(webDirectory,project,version,str_c(fname,".Rmd"))))

    if(grepl(class(test),"try-error")){
      failed = c(failed, udsn[i])
    }else{
      print(str_c("http://lipdverse.org/",project,"/",version,"/",fname,".html"))
      D[[map.meta$dataSetName[i]]]$lipdverseLink <- str_c("http://lipdverse.org/",project,"/",version,"/",fname,".html")
    }
    writeLipd(D[[map.meta$dataSetName[i]]],path = file.path(webDirectory,project,version))

 # }
  #copy the lipd file if it's not already there
  # if(!file.exists(file.path(webDirectory,project,version,str_c(fname,".lpd")))){
  #   file.copy(from = str_c(lipdDir,fname,".lpd"),to = file.path(webDirectory,project))
  # }
}
setwd(file.path(webDirectory,project,version))
zip(zipfile = file.path(webDirectory,project,version,str_c(project,version,".zip")),files = list.files(file.path(webDirectory,project,version),pattern= "*.lpd"))

#write out failed somewhere
write.table(x = failed,file = file.path(webDirectory,project,version,"failedLipdversePage.txt"),col.names = FALSE,row.names = FALSE )

#if current version, copy into current_version folder
if(currentVersion){
  unlink(file.path(webDirectory,project,"current_version"),force = TRUE,recursive = TRUE)
  dir.create(file.path(webDirectory,project,"current_version"))
  file.copy(file.path(webDirectory,project,version,.Platform$file.sep), file.path(webDirectory,project,"current_version"), recursive=TRUE,overwrite = TRUE)
}





}


#' Calculate MD5 for a zipfile of a directory
#'
#' @param thisDir
#' @param pattern
#'
#' @return MD5 string
#' @export
#' @importFrom tools md5sum
#'
#' @examples
directoryMD5 <- function(thisDir,pattern = "*.lpd"){
td <- getwd()
setwd(thisDir)
zip(zipfile = file.path(thisDir,"test.zip"),files = list.files(thisDir,pattern= pattern))
thisMD5 <- tools::md5sum("test.zip")
unlink("test.zip")
setwd(td)
return(thisMD5)
}

