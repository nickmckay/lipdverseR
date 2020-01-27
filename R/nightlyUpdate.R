#' Get google drive file update time
#'
#' @param googId Google drive file key
#' @param tzone timezone
#' @import googledrive lubridate
#' @return
#' @export
#'
#' @examples
googleDriveUpdateTime <- function(googId,tzone = "UTC"){
  #money sheet update
  info <- googledrive::drive_get(googledrive::as_id(googId))
  mtime <- info[3]$drive_resource[[1]]$modifiedTime
  return(lubridate::with_tz(lubridate::ymd_hms(mtime),tzone = tzone))
}


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
  qcUpdate <- googleDriveUpdateTime(qcId)

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
#' @param lastUpdateId google sheets ID for the last version
#' @param updateWebpages update lipdverse webpages (default = TRUE). Usually TRUE unless troubleshooting.
#' @param googEmail google user ID
#' @import purrr
#' @import googlesheets4
#' @import readr
#' @import lipdR
#' @import geoChronR
#' @export
updateProject <- function(project,lipdDir,webDirectory,qcId,lastUpdateId,versionMetaId = "1OHD7PXEQ_5Lq6GxtzYvPA76bpQvN1_eYoFR0X80FIrY",googEmail = NULL,updateWebpages = TRUE,standardizeTerms = TRUE,ageOrYear = "age"){
  #
  # project <- "globalHolocene"
  # lipdDir <- "~/Dropbox/HoloceneLiPDLibrary/masterDatabase/"
  # qcId <- "1JEm791Nhd4fUuyqece51CSlbR2A2I-pf8B0kFgwynug"
  # webDirectory <- "/Users/npm4/GitHub/lipdverse/html"
  # updateWebpages = FALSE
  # lastUpdateId = "1qLRMCfDMbTyffJBWlIj3Zw4CAhJY2SECIY-ckcZ2Wak"
  # googEmail = "nick.mckay2@gmail.com"


  #authorize google
  googlesheets4::sheets_auth(email = googEmail,cache = TRUE)
  #check if update is necessary
  toUpdate <- updateNeeded(project,webDirectory,lipdDir,qcId,googEmail = googEmail)

  if(!toUpdate){
    return("No update needed")
  }
  #an update is needed!!!

  #1. load in (potentially updated) files
  filesToUltimatelyDelete <- lipdR:::get_lipd_paths(lipdDir)
  D <- lipdR::readLipd(lipdDir)

  #make sure that primary chronologies are named appropriately
  D <- purrr::map(D,renamePrimaryChron)


  if(standardizeTerms){
    D <- purrr::map(D,hasDepth)
    D <- purrr::map(D,nUniqueAges)
    D <- purrr::map(D,nGoodAges)
    D <- purrr::map(D,nOtherAges)
    D <- purrr::map(D,fixExcelIssues)
    D <- purrr::map(D,standardizeChronVariableNames)
  }

  #1a. Screen by some criterion...



  #check for TSid
  TS <- lipdR::extractTs(D)

  #create grouping terms for later standardization

  #TO DO!# remove entries that don't fall into the groups/lumps!
  if(standardizeTerms){
    #proxy lumps
    pl <- geoChronR::pullTsVariable(TS,"paleoData_proxy")
    TS <- geoChronR::pushTsVariable(TS,"paleoData_proxyLumps",pl,createNew = TRUE)

    #inferred material
    pl <- geoChronR::pullTsVariable(TS,"paleoData_inferredMaterial")
    TS <- geoChronR::pushTsVariable(TS,"paleoData_inferredMaterialGroup",pl,createNew = TRUE)

    #interpretation variable groups

    pl <- geoChronR::pullTsVariable(TS,"interpretation1_variable")
    TS <- geoChronR::pushTsVariable(TS,"interpretation1_variableGroup",pl,createNew = TRUE)

    pl <- geoChronR::pullTsVariable(TS,"interpretation2_variable")
    TS <- geoChronR::pushTsVariable(TS,"interpretation2_variableGroup",pl,createNew = TRUE)

    pl <- geoChronR::pullTsVariable(TS,"interpretation3_variable")
    TS <- geoChronR::pushTsVariable(TS,"interpretation3_variableGroup",pl,createNew = TRUE)



    #Do some cleaning
    TS <- standardizeTsValues(TS)
    TS <- fix_pubYear(TS)
    TS <- fixKiloyearsTs(TS)
    TS <- purrr::map(TS,removeEmptyInterpretationsFromTs)
  }

  #get some relevant information
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
    ntsid <- purrr::map_chr(et,lipdR::createTSid)
    TSid[et] <- ntsid
    TS <- geoChronR::pushTsVariable(TS,variable = "paleoData_TSid",vec = TSid)
  }



  sTS <- lipdR::splitInterpretationByScope(TS)

  #2. Create a new qc sheet from files
  qcC <- createQCdataFrame(sTS,templateId = qcId,ageOrYear = ageOrYear)
  readr::write_csv(qcC,path = file.path(webDirectory,project,projVersion,"qcTs.csv"))

  #3. Get the updated QC sheet from google
  #first, lock editing
  #googledrive::drive_share(as_id(qcId),role = "reader", type = "anyone")



  #now get the file
  qcB <- getGoogleQCSheet(qcId)
  readr::write_csv(qcB,path = file.path(webDirectory,project,projVersion,"qcGoog.csv"))

  lu <- getGoogleQCSheet(lastUpdateId)
  readr::write_csv(lu,file.path(webDirectory,project,"lastUpdate.csv"))

  #4. Load in the old QC sheet (from last update), and merge with new ones
  rosetta <- lipdverseR::rosettaStone()
  qcA <- readr::read_csv(file.path(webDirectory,project,"lastUpdate.csv"),guess_max = Inf) %>%
    purrr::map_df(lipdverseR::replaceSpecialCharacters,rosetta)

  qcB <- readr::read_csv(file.path(webDirectory,project,projVersion,"qcGoog.csv"),guess_max = Inf) %>%
    purrr::map_df(lipdverseR::replaceSpecialCharacters,rosetta)
  qcC <- readr::read_csv(file.path(webDirectory,project,projVersion,"qcTs.csv"),guess_max = Inf) %>%
    purrr::map_df(lipdverseR::replaceSpecialCharacters,rosetta)
  qc <- daff::merge_data(parent = qcA,a = qcB,b = qcC)

  #this should fix conflicts that shouldnt exist
  #qc <- resolveDumbConflicts(qc)

  #find differences for log
  diff <- daff::diff_data(qcA,qc,ids = "TSid",ignore_whitespace = TRUE,columns_to_ignore = "link to lipdverse",never_show_order = TRUE)
  daff::render_diff(diff,file = file.path(webDirectory,project,projVersion,"metadataChangelog.html"),title = paste("Metadata changelog:",project,projVersion),view = FALSE)


  #remove duplicate rows
  qc <- dplyr::distinct(qc)


  #5. Update sTS from merged qc
  nsTS <- updateFromQC(sTS,qc)
  nTS <- combineInterpretationByScope(nsTS)


  if(standardizeTerms){#To do: #make this its own function
    #proxy lumps
    pl <- geoChronR::pullTsVariable(TS,"paleoData_proxy")
    TS <- geoChronR::pushTsVariable(TS,"paleoData_proxyLumps",pl,createNew = TRUE)

    #inferred material
    pl <- geoChronR::pullTsVariable(TS,"paleoData_inferredMaterial")
    TS <- geoChronR::pushTsVariable(TS,"paleoData_inferredMaterialGroup",pl,createNew = TRUE)

    #interpretation variable groups

    pl <- geoChronR::pullTsVariable(TS,"interpretation1_variable")
    TS <- geoChronR::pushTsVariable(TS,"interpretation1_variableGroup",pl,createNew = TRUE)

    pl <- geoChronR::pullTsVariable(TS,"interpretation2_variable")
    TS <- geoChronR::pushTsVariable(TS,"interpretation2_variableGroup",pl,createNew = TRUE)

    pl <- geoChronR::pullTsVariable(TS,"interpretation3_variable")
    TS <- geoChronR::pushTsVariable(TS,"interpretation3_variableGroup",pl,createNew = TRUE)



    #Do some cleaning
    TS <- standardizeTsValues(TS)
    TS <- fix_pubYear(TS)
    TS <- fixKiloyearsTs(TS)
    TS <- purrr::map(TS,removeEmptyInterpretationsFromTs)
  }

  #5c rebuild database
  nD <- collapseTs(nTS)

  #5d clean D
  if(standardizeTerms){
    nDt <- purrr::map(nD,removeEmptyPubs)
    if(class(nDt) == "list"){
      nD <- nDt
    }
  }



  #6 Update lipdverse
  if(updateWebpages){
    createProjectDashboards(nD,nTS,webDirectory,project,projVersion,currentVersion = TRUE)

    #load back in files
    DF <- readLipd(file.path(webDirectory,project,projVersion))


    createSerializations(D = DF,webDirectory,project,projVersion)

  }else{
    DF <- nD
  }
  TSF <- extractTs(DF)
  sTSF <- splitInterpretationByScope(TSF)
  qcF <- createQCdataFrame(sTSF,templateId = qcId,ageOrYear = ageOrYear)


  #7 Update QC sheet on google (and make a lastUpdate.csv file)

  qc2w <- qcF
  qc2w[is.na(qc2w)] <- ""

  readr::write_csv(qc2w,path = file.path(webDirectory,project,"newLastUpdate.csv"))

  googledrive::drive_update(file = googledrive::as_id(lastUpdateId),media = file.path(webDirectory,project,"newLastUpdate.csv"))
  newName <- str_c(project," v.",projVersion," QC sheet")

  newName <- str_c(project," v.",projVersion," QC sheet")

  googledrive::drive_update(file = googledrive::as_id(qcId),media = file.path(webDirectory,project,"newLastUpdate.csv"),name = newName)
  googlesheets4::sheets_auth(email = googEmail,cache = TRUE)

  #8 write lipd files
  unlink(x = filesToUltimatelyDelete,force = TRUE, recursive = TRUE)

  DF <- purrr::map(DF,removeEmptyPubs)

  writeLipd(DF,path = lipdDir,removeNamesFromLists = TRUE)


  #9 update the google version file
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



  #check for differences in dsns
  dsndiff <- filter(versionDf,project == (!!project)) %>%
    filter(versionCreated == max(versionCreated))

  oldDsns <- stringr::str_split(dsndiff$dsns,pattern = "[|]",simplify = T)
  newDsns <- stringr::str_split(newRow$dsns,pattern = "[|]",simplify = T)

  newRow$`dataSets removed` <- paste(setdiff(oldDsns,newDsns),collapse = "|")
  newRow$`dataSets added` <- paste(setdiff(newDsns,oldDsns),collapse = "|")

  nvdf <- dplyr::bind_rows(versionDf,newRow)

  readr::write_csv(nvdf,path = file.path(tempdir(),"versTemp.csv"))

  googledrive::drive_update(media = file.path(tempdir(),"versTemp.csv"),file = googledrive::as_id(versionMetaId),name = "lipdverse versioning spreadsheet")
  #give permissions back
  #drive_share(as_id(qcId),role = "writer", type = "user",emailAddress = "")

}

#' create serializations of a database in R, matlab and python
#'
#' @param D
#' @param matlabUtilitiesPath
#' @param matlabPath
#' @param webDirectory
#' @param project
#' @param projVersion
#' @param python3Path
#'
#' @import stringr
#' @import lipdR
#' @import readr
#' @description creates serialization; requires that Matlab and Python be installed, along with lipd utilities for those languages.
#' @return
#' @export
createSerializations <- function(D,webDirectory,project,projVersion,matlabUtilitiesPath = "/Users/npm4/GitHub/LiPD-utilities/Matlab",matlabPath = "/Applications/MATLAB_R2015b.app/bin/matlab", python3Path="/Users/npm4/anaconda/bin/python3"){
  #create serializations for web
  #R

  TS <- extractTs(D)
  sTS <- splitInterpretationByScope(TS)
  save(list = c("D","TS","sTS"),file = file.path(webDirectory,project,projVersion,stringr::str_c(project,projVersion,".RData")))


  #matlab
  mfile <- stringr::str_c("addpath(genpath('",matlabUtilitiesPath,"'));\n") %>%
    stringr::str_c("D = readLiPD('",file.path(webDirectory,project,projVersion),"');\n") %>%
    stringr::str_c("TS = extractTs(D);\n") %>%
    stringr::str_c("sTS = splitInterpretationByScope(TS);\n") %>%
    stringr::str_c("save ",file.path(webDirectory,project,projVersion,stringr::str_c(project,projVersion,".mat")),' D TS sTS\n') %>%
    stringr::str_c("exit")

  #write the file
  readr::write_file(mfile,path = file.path(webDirectory,project,projVersion,"createSerialization.m"))

  #run the file
  system(stringr::str_c(matlabPath," -nodesktop -nosplash -nodisplay -r \"run('",file.path(webDirectory,project,projVersion,"createSerialization.m"),"')\""))


  #Python
  pyfile <- "import lipd\n" %>%
    stringr::str_c("import pickle\n") %>%
    stringr::str_c("D = lipd.readLipd('",file.path(webDirectory,project,projVersion),"/')\n") %>%
    stringr::str_c("TS = lipd.extractTs(D)\n") %>%
    stringr::str_c("filetosave = open('",file.path(webDirectory,project,projVersion,stringr::str_c(project,projVersion,".pkl'")),",'wb')\n") %>%
    stringr::str_c("all_data = {}\n") %>%
    stringr::str_c("all_data['D'] = D\n") %>%
    stringr::str_c("all_data['TS']  = TS\n") %>%
    stringr::str_c("pickle.dump(all_data, filetosave,protocol = 2)\n") %>%
    stringr::str_c("filetosave.close()")

  #write the file
  readr::write_file(pyfile,path = file.path(webDirectory,project,projVersion,"createSerialization.py"))

  #run the file
  system(str_c(python3Path, " ",file.path(webDirectory,project,projVersion,"createSerialization.py")))

}


