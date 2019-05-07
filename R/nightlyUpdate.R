#' Check to see if a project needs to be updated
#'
#' @param project
#' @param webDirectory
#' @param lipdDir
#' @param qcId
#' @param versionMetaId
#' @import googlesheets4
#' @import magrittr
#' @import dplyr
#' @import googledrive
#' @import lubridate
#'
#' @return TRUE or FALSE
#' @export
updateNeeded <- function(project,webDirectory,lipdDir,qcId,versionMetaId = "1OHD7PXEQ_5Lq6GxtzYvPA76bpQvN1_eYoFR0X80FIrY",googEmail = NULL){

  #compare files with MD5s
  # currentMD5 <- directoryMD5(lipdDir)
  # dir(lipdDir)
  #
  # lastMD5 <- directoryMD5(file.path(webDirectory,project,"current_version"))
  #
  googlesheets4::sheets_auth(email = googEmail,cache = TRUE)


  #compare QC update times
  versionSheet <- googlesheets4::read_sheet(googledrive::as_id(versionMetaId)) %>%
    dplyr::filter(project == (!!project)) %>%
    dplyr::arrange(desc(versionCreated))

  lastUpdate <- lubridate::ymd_hms(versionSheet$versionCreated[1])
  lastMD5 <- versionSheet$`zip MD5`[1]

  currentMD5 <- directoryMD5(lipdDir)

  filesNeedUpdating <- TRUE
  if(lastMD5 == currentMD5){
    filesNeedUpdating <- FALSE
  }

  #most recent file edit time
  lastMod <- purrr::map(list.files(lipdDir,pattern = "*.lpd",full.names = TRUE),file.mtime )
  lastMod <- lubridate::with_tz(lubridate::ymd_hms(lastMod[[which.max(unlist(lastMod))]],tz = "America/Phoenix"),tzone = "UTC")


#  check based on folder modification time
#   filesNeedUpdating <- TRUE
#   if(lastUpdate > lastMod){
#     filesNeedUpdating <- FALSE
#   }

  #most recent QC update
  info <- googledrive::drive_get(googledrive::as_id(qcId))
  qcUpdate <- info[3]$drive_resource[[1]]$modifiedTime
  qcUpdate <- lubridate::with_tz(lubridate::ymd_hms(qcUpdate),tzone = "UTC")

  qcNeedsUpdating <- TRUE
  if(lastUpdate > qcUpdate){
    qcNeedsUpdating <- FALSE
  }

  if(qcNeedsUpdating | filesNeedUpdating){
    needsUpdating <- TRUE
  }else{
    needsUpdating <- FALSE
  }

  return(needsUpdating)

}

#' Title
#'
#' @param project project name
#' @param udsn a vector of dataset names in the project
#' @param versionMetaId ID of the versioning qc sheet
#' @param googEmail google user ID
#' @description Ticks the version of a database for you. Assumes that a change is necessary.
#' @import googlesheets4
#' @import magrittr
#' @import dplyr
#' @import googledrive
#' @import stringr
#' @return the new version string
#' @export
#'
#' @examples
tickVersion <- function(project,udsn,versionMetaId = "1OHD7PXEQ_5Lq6GxtzYvPA76bpQvN1_eYoFR0X80FIrY",googEmail = NULL){

  googlesheets4::sheets_auth(email = googEmail,cache = TRUE)

  #get last versions udsn
  versionSheet <- googlesheets4::read_sheet(googledrive::as_id(versionMetaId)) %>%
    dplyr::filter(project == (!!project)) %>%
    dplyr::arrange(desc(versionCreated))

  lastUdsn <- versionSheet$dsns[1]

  #and the new udsn
  thisUdsn <- paste(unique(udsn),collapse = "|")

  if(lastUdsn==thisUdsn){
    #then tick metadata
    p <- versionSheet$publication[1]
    d <- versionSheet$dataset[1]
    m <- versionSheet$metadata[1]+1
  }else{
    p <- versionSheet$publication[1]
    d <- versionSheet$dataset[1]+1
    m <- 0
  }

  newVers <- str_c(p,d,m,sep = "_")
  return(newVers)

}



#' Do a complete update to a project
#'
#' @param project project name
#' @param lipdDir authority directory for a lipd file
#' @param webDirectory directory for webserver
#' @param qcId google sheets ID for the qc sheet
#' @param updateWebpages update lipdverse webpages (default = TRUE). Usually TRUE unless troubleshooting.
#' @param googEmail google user ID
#' @import purrr
#' @import googlesheets4
#' @import readr
#' @import lipdR
#' @import geoChronR
#' @export
updateProject <- function(project,lipdDir,webDirectory,qcId,versionMetaId = "1OHD7PXEQ_5Lq6GxtzYvPA76bpQvN1_eYoFR0X80FIrY",googEmail = NULL,updateWebpages = TRUE){
#
# project <- "test"
# lipdDir <- "~/Dropbox/LiPD/namChironomid/"
# qcId <- "1lVsU5v3V1Cmq1xOO6LVchO2M2K45_QHv5SiqPb3xIaQ"
# webDirectory <- "/Users/npm4/GitHub/lipdverse/html"

#authorize google
googlesheets4::sheets_auth(email = googEmail,cache = TRUE)
#check if update is necessary
toUpdate <- updateNeeded(project,webDirectory,lipdDir,qcId,googEmail = googEmail)

if(!toUpdate){
  return("No update needed")
}
#an update is needed!!!

#1. load in (potentially updated) files
D <- readLipd(lipdDir)
#check for TSid
TS <- extractTs(D)
TSid <- geoChronR::pullTsVariable(TS,"paleoData_TSid")
udsn <- unique(geoChronR::pullTsVariable(TS,"dataSetName"))

#1b. New version name
projVersion <- tickVersion(project,udsn,googEmail = googEmail)

#setup new version
if(!dir.exists(file.path(webDirectory,project))){
  dir.create(file.path(webDirectory,project))
}

if(!dir.exists(file.path(webDirectory,project,projVersion))){
  dir.create(file.path(webDirectory,project,projVersion))
}


#create TSids if needed
et <- which(is.na(TSid))
if(length(et) > 0){
ntsid <- purrr::map_chr(et,createTSid)
TSid[et] <- ntsid
TS <- pushTsVariable(TS,variable = "paleoData_TSid",vec = TSid)
}

sTS <- splitInterpretationByScope(TS)

#2. Create a new qc sheet from files
qcC <- createQCdataFrame(sTS,templateId = qcId)
readr::write_csv(qcC,path = file.path(webDirectory,project,projVersion,"qcTs.csv"))

#3. Get the updated QC sheet from google
drive_share(as_id(qcId),role = "reader", type = "anyone")

#first, lock editing


#now get the file
qcB <- getGoogleQCSheet(qcId)
readr::write_csv(qcB,path = file.path(webDirectory,project,projVersion,"qcGoog.csv"))


#4. Load in the old QC sheet (from last update), and merge with new ones
qcA <- readr::read_csv(file.path(webDirectory,project,"lastUpdate.csv"))

qcB <- readr::read_csv(file.path(webDirectory,project,projVersion,"qcGoog.csv"))
qcC <- readr::read_csv(file.path(webDirectory,project,projVersion,"qcTs.csv"))
qc <- daff::merge_data(parent = qcA,a = qcB,b = qcC)

#remove duplicate rows
qc <- dplyr::distinct(qc)


#5. Update sTS from merged qc
nsTS <- updateFromQC(sTS,qc)
nTS <- combineInterpretationByScope(nsTS)
nD <- collapseTs(nTS)

#6 Update lipdverse
if(updateWebpages){
createProjectDashboards(nD,nTS,webDirectory,project,projVersion,currentVersion = TRUE)

#load back in files
DF <- readLipd(file.path(webDirectory,project,projVersion))
}else{
  DF <- nD
}
TSF <- extractTs(DF)
sTSF <- splitInterpretationByScope(TSF)
qcF <- createQCdataFrame(sTSF,templateId = qcId)


#7 Update QC sheet on google (and make a lastUpdate.csv file)

qc2w <- qcF
qc2w[is.na(qc2w)] <- ""
readr::write_csv(qc2w,path = file.path(webDirectory,project,"lastUpdate.csv"))


newName <- str_c(project," v.",projVersion," QC sheet")

googledrive::drive_update(file = googledrive::as_id(qcId),media = file.path(webDirectory,project,"lastUpdate.csv"),name = newName)

#8 write lipd files
unlink(x = list.files(lipdDir,pattern = "*.lpd",full.names = TRUE),force = TRUE, recursive = TRUE)
writeLipd(DF,path = lipdDir)


#9 update the google version file
googlesheets4::sheets_auth(email = googEmail,cache = TRUE)
versionDf <- googlesheets4::read_sheet(googledrive::as_id(versionMetaId))
versionDf$versionCreated <- lubridate::ymd_hms(versionDf$versionCreated)


newRow <- versionDf[1,]

newRow$project <- project
pdm <- as.numeric(unlist(str_split(projVersion,"_")))
newRow$publication <- pdm[1]
newRow$dataset <- pdm[2]
newRow$metadata <- pdm[3]
newRow$dsns <- paste(unique(geoChronR::pullTsVariable(TSF,"dataSetName")),collapse = "|")
newRow$versionCreated <- lubridate::now(tzone = "UTC")
newRow$`zip MD5` <- directoryMD5(lipdDir)

nvdf <- dplyr::bind_rows(versionDf,newRow)

readr::write_csv(nvdf,path = file.path(tempdir(),"versTemp.csv"))

googledrive::drive_update(media = file.path(tempdir(),"versTemp.csv"),file = googledrive::as_id(versionMetaId),name = "lipdverse versioning spreadsheet")
#give permissions back
drive_share(as_id(qcId),role = "writer", type = "anyone")

}




