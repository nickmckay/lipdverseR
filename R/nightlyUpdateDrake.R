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
  googlesheets4::sheets_auth(email = googEmail)


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
#' @param versionMetaId ID of the versioning qc sheet
#' @param qcIc dataSetNames in this compilation from teh QC sheet
#' @param tsIc dataSetNames in the last compilation from the files
#' @param googEmail google user ID
#'
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
tickVersion <- function(project,qcIc,tsIc,versionMetaId = "1OHD7PXEQ_5Lq6GxtzYvPA76bpQvN1_eYoFR0X80FIrY",googEmail = NULL){

  googlesheets4::sheets_auth(email = googEmail)

  #get last versions udsn
  versionSheet <- googlesheets4::read_sheet(googledrive::as_id(versionMetaId)) %>%
    dplyr::filter(project == (!!project)) %>%
    dplyr::arrange(desc(versionCreated))

  lastUdsn <- sort(tsIc)

  #and the new udsn
  thisUdsn <- sort(qcIc)

  if(all(lastUdsn==thisUdsn)){
    #then tick metadata
    p <- versionSheet$publication[1]
    d <- versionSheet$dataset[1]
    m <- versionSheet$metadata[1]+1
  }else{
    p <- versionSheet$publication[1]
    d <- versionSheet$dataset[1]+1
    m <- 0
  }

  newVers <- stringr::str_c(p,d,m,sep = "_")
  return(newVers)

}


#' Get the most recent version of the compilation (before updating)
#'
#' @param project project name
#' @param udsn a vector of dataset names in the project
#' @param versionMetaId ID of the versioning qc sheet
#' @param googEmail google user ID
#' @description Gets the last version of the database (before updating)
#' @import googlesheets4
#' @import magrittr
#' @import dplyr
#' @import googledrive
#' @import stringr
#' @return the new version string
#' @export
#'
#' @examples
lastVersion <- function(project,versionMetaId = "1OHD7PXEQ_5Lq6GxtzYvPA76bpQvN1_eYoFR0X80FIrY",googEmail = NULL){

  googlesheets4::sheets_auth(email = googEmail)

  #get last versions udsn
  versionSheet <- googlesheets4::read_sheet(googledrive::as_id(versionMetaId)) %>%
    dplyr::filter(project == (!!project)) %>%
    dplyr::arrange(desc(versionCreated))

  p <- versionSheet$publication[1]
  d <- versionSheet$dataset[1]
  m <- versionSheet$metadata[1]

  lastVers <- stringr::str_c(p,d,m,sep = "_")
  return(lastVers)

}


assignVariablesFromList <- function(params,env = parent.env(environment())){
  for(i in 1:length(params)){
    assign(names(params)[i],params[[i]],envir = env)
  }
}


#' Build parameters
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
buildParams <- function(project,
                        lipdDir,
                        webDirectory,
                        qcId,
                        lastUpdateId,
                        versionMetaId = "1OHD7PXEQ_5Lq6GxtzYvPA76bpQvN1_eYoFR0X80FIrY",
                        googEmail = NULL,
                        updateWebpages = TRUE,
                        standardizeTerms = TRUE,
                        ageOrYear = "age",
                        recreateDataPages = FALSE,
                        restrictWebpagesToCompilation = TRUE,
                        serialize = TRUE,
                        projVersion = NA){

  an <- ls()
  av <- purrr::map(an,~eval(parse(text = .x))) %>% setNames(an)
  return(av)
}



#' Check if an update is needed
#'
#' @param params
#'
#' @return
#' @export
#'
#' @examples
checkIfUpdateNeeded <- function(params){

  #parse parameters
  assignVariablesFromList(params)



  if(is.na(projVersion)){#skip check if new version is specified
    #check if update is necessary
    toUpdate <- updateNeeded(project,webDirectory,lipdDir,qcId,googEmail = googEmail)

    if(!toUpdate){
      return("No update needed")
    }else{
      return("Update needed")
    }
  }
}


#' Load in new data
#'
#' @param params
#'
#' @return
#' @export
loadInUpdatedData <- function(params){

  assignVariablesFromList(params)


  #0. Figure out which datasets to load based on QC sheet.
  dscomp <- googlesheets4::read_sheet(ss = qcId,sheet = "datasetsInCompilation") %>%
    dplyr::filter(inComp != "FALSE")

  filesToConsider <- file.path(lipdDir, paste0(dscomp$dsn,".lpd"))
  filesToUltimatelyDelete <- filesToConsider

  #1. load in (potentially updated) files
  D <- lipdR::readLipd(filesToConsider)


  #create datasetIds for records that don't have them
  for(d in 1:length(D)){
    if(is.null(D[[d]]$datasetId)){
      D[[d]]$datasetId <- createDatasetId()
    }
    #check for chronMeasurementTable and fix
    if(!is.null(D[[d]]$chronData[[1]]$chronMeasurementTable)){
      for(ccic in 1:length(D[[d]]$chronData)){
        D[[d]]$chronData[[ccic]]$measurementTable <- D[[d]]$chronData[[ccic]]$chronMeasurementTable
        D[[d]]$chronData[[ccic]]$chronMeasurementTable <- NULL
      }
    }

    #check for changelog and fix
    if(is.null(D[[d]]$changelog)){
      D[[d]] <- initializeChangelog(D[[d]])
    }
  }


  Dloaded <- D#store for changelogging

  dsidsOriginal <- tibble::tibble(datasetId = purrr::map_chr(D,"datasetId"),
                                  dataSetNameOrig = purrr::map_chr(D,"dataSetName"),
                                  dataSetVersion = purrr::map_chr(D,getVersion))

  #make sure that primary chronologies are named appropriately
  D <- purrr::map(D,renamePrimaryChron)

  if(standardizeTerms){
    D <- purrr::map(D,cleanOriginalDataUrl)
    D <- purrr::map(D,hasDepth)
    D <- purrr::map(D,nUniqueAges)
    D <- purrr::map(D,nGoodAges)
    D <- purrr::map(D,nOtherAges)
    # D <- purrr::map(D,fixExcelIssues)
    D <- purrr::map(D,standardizeChronVariableNames)
  }

  #1a. Screen by some criterion...

  #check for TSid
  TS <- lipdR::extractTs(D)

  #create grouping terms for later standardization

  #TO DO!# remove entries that don't fall into the groups/lumps!
  if(standardizeTerms){
    #Do some cleaning
    TS <- standardizeTsValues(TS)
    TS <- fix_pubYear(TS)
    TS <- fixKiloyearsTs(TS)
    TS <- purrr::map(TS,removeEmptyInterpretationsFromTs)
  }

  #get some relevant information
  TSid <- lipdR::pullTsVariable(TS,"paleoData_TSid")
  udsn <- unique(lipdR::pullTsVariable(TS,"dataSetName"))


  data <- list(Dloaded = Dloaded ,
               D = D,
               TS = TS,
               TSid = TSid,
               filesToUltimatelyDelete = filesToUltimatelyDelete,
               dsidsOriginal = dsidsOriginal,
               udsn = udsn)


  return(data)

}


#' Get QC
#'
#' @param params
#' @param data
#'
#' @return
#' @export
getQcInfo <- function(params,data){

  assignVariablesFromList(params)
  assignVariablesFromList(data)

  #get the google qc sheet
  qcB <- getGoogleQCSheet(qcId)

  if(!any(names(qcB)=="changelogNotes")){
    qcB$changelogNotes <- NA
  }

  #pull out changelog notes
  clNotes <- qcB %>%
    dplyr::select(dataSetName,TSid,changelogNotes) %>%
    dplyr::filter(!is.na(changelogNotes)) %>%
    dplyr::group_by(dataSetName) %>%
    dplyr::summarize(changes = paste(paste(TSid,changelogNotes,sep = ": "),collapse = "; ")) %>%
    dplyr::rename(dataSetNameOrig = dataSetName)

  #then remove that column
  qcB <- dplyr::select(qcB,-changelogNotes)

  data$dsidsOriginal <- data$dsidsOriginal %>%
    dplyr::left_join(clNotes,by = "dataSetNameOrig")

  #1b. New version name
  lastProjVersion <- lastVersion(project,googEmail = googEmail)

  if(is.na(projVersion)){
    #qc in compilation
    qcIc <- qcB %>%
      filter(inThisCompilation == TRUE) %>%
      select(dataSetName) %>%
      unique()

    qcIc <- qcIc$dataSetName


    inLast <- inThisCompilation(TS,project,lastProjVersion)
    tsIci <- which(purrr::map_lgl(inLast,isTRUE))
    tsIc <- unique(lipdR::pullTsVariable(TS,"dataSetName")[tsIci])


    projVersion <- tickVersion(project,qcIc,tsIc,googEmail = googEmail)
  }


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
    ntsid <- unlist(purrr::rerun(length(et),lipdR::createTSid()))
    TSid[et] <- ntsid
    TS <- lipdR::pushTsVariable(TS,variable = "paleoData_TSid",vec = TSid)
  }

  #check for duplicate TSids
  while(any(duplicated(TSid))){
    wd <- which(duplicated(TSid))
    dtsid <- paste0(TSid[wd],"-dup")
    TSid[wd] <- dtsid
    TS <- lipdR::pushTsVariable(TS,variable = "paleoData_TSid",vec = TSid)
  }



  sTS <- lipdR::splitInterpretationByScope(TS)

  data$TS <- TS

  newData <- list(qcB = qcB,
                  clNotes = clNotes,
                  projVersion = projVersion,
                  lastProjVersion = lastProjVersion,
                  sTS = sTS)

  data <- append(data,newData)

  return(data)
}

#' Create QC sheet from data
#'
#' @param params
#' @param data
#'
#' @return
#' @export
createQcFromFile <- function(params,data){
  assignVariablesFromList(params)
  assignVariablesFromList(data)

  #2. Create a new qc sheet from files
  qcC <- createQCdataFrame(sTS,templateId = qcId,ageOrYear = ageOrYear,compilationName = project,compVersion = lastProjVersion)
  readr::write_csv(qcC,path = file.path(webDirectory,project,projVersion,"qcTs.csv"))

  #3. Get the updated QC sheet from google
  #first, lock editing
  #googledrive::drive_share(as_id(qcId),role = "reader", type = "anyone")

  #check for duplicate TSids
  while(any(duplicated(qcB$TSid))){
    wd <- which(duplicated(qcB$TSid))
    dtsid <- paste0(qcB$TSid[wd],"-dup")
    qcB$TSid[wd] <- dtsid
  }

  readr::write_csv(qcB,path = file.path(webDirectory,project,projVersion,"qcGoog.csv"))

  lu <- getGoogleQCSheet(lastUpdateId)
  readr::write_csv(lu,file.path(webDirectory,project,"lastUpdate.csv"))

  data$qcC <- qcC
  return(data)
}

#' Merge sources
#'
#' @param params
#' @param data
#'
#' @return
#' @export
mergeQcSheets <- function(params,data){
  assignVariablesFromList(params)
  assignVariablesFromList(data)
  #4. Load in the old QC sheet (from last update), and merge with new ones
  rosetta <- lipdverseR::rosettaStone()
  qcA <- readr::read_csv(file.path(webDirectory,project,"lastUpdate.csv"),guess_max = Inf) %>%
    purrr::map_df(lipdverseR::replaceSpecialCharacters,rosetta)

  qcB <- readr::read_csv(file.path(webDirectory,project,projVersion,"qcGoog.csv"),guess_max = Inf) %>%
    purrr::map_df(lipdverseR::replaceSpecialCharacters,rosetta)
  qcC <- readr::read_csv(file.path(webDirectory,project,projVersion,"qcTs.csv"),guess_max = Inf) %>%
    purrr::map_df(lipdverseR::replaceSpecialCharacters,rosetta)


  #qc <- daff::merge_data(parent = qcA,a = qcB,b = qcC) Old way
  #NPM: 2.20.20 added to help merge_data work as desired

  #shuffle in
  dBC <- dplyr::anti_join(qcB,qcC,by = "TSid")
  dCB <- dplyr::anti_join(qcC,qcB,by = "TSid")
  dCA <- dplyr::anti_join(qcC,qcA,by = "TSid")

  qcA2 <- dplyr::bind_rows(qcA,dCA)
  qcB2 <- dplyr::bind_rows(qcB,dCB)
  qcC2 <- dplyr::bind_rows(qcC,dBC)

  #check once more
  dBA <- dplyr::anti_join(qcB,qcA2,by = "TSid")
  qcA2 <- dplyr::bind_rows(qcA2,dBA)


  #arrange by qcB TSid
  miA <- match(qcB2$TSid,qcA2$TSid)
  miC <- match(qcB2$TSid,qcC2$TSid)

  qcA <- qcA2[miA,]

  qcC <- qcC2[miC,]
  qcB <- qcB2

  #turn all NULLs and blanks to NAs
  qcA[is.null(qcA) | qcA == ""] <- NA
  qcB[is.null(qcB) | qcB == ""] <- NA
  qcC[is.null(qcC) | qcC == ""] <- NA

  #prep inThisCompilation
  qcA$inThisCompilation[is.na(qcA$inThisCompilation)] <- FALSE
  qcB$inThisCompilation[is.na(qcB$inThisCompilation)] <- FALSE
  qcC$inThisCompilation[is.na(qcC$inThisCompilation)] <- FALSE

  #find all TRUE in B and apply to C (since they should only be changed in B)
  bf <- qcB %>%
    filter(inThisCompilation == "TRUE")

  cfi <- which(qcC$TSid %in% bf$TSid)
  qcC$inThisCompilation[cfi] <- "TRUE"

  qc <- daff::merge_data(qcA,qcB,qcC)

  if(any(names(qc) == "inThisCompilation")){
    #check for conflicts in "inThisCompilation"
    #this is especially important when first starting this variable
    #default to google qc sheet (qcB)
    shouldBeTrue <- which(qc$inThisCompilation == "((( null ))) TRUE /// FALSE")
    shouldBeFalse <- which(qc$inThisCompilation == "((( null ))) FALSE /// TRUE")
    qc$inThisCompilation[shouldBeTrue] <- "TRUE"
    qc$inThisCompilation[shouldBeFalse] <- "FALSE"
  }



  #this should fix conflicts that shouldnt exist
  #qc <- resolveDumbConflicts(qc)

  #remove fake conflicts
  qc <- purrr::map_dfc(qc,removeFakeConflictsCol)

  #remove duplicate rows
  qc <- dplyr::distinct(qc)

  data$qc <- qc
  data$qcA <- qcA
  return(data)
}


#' updateTsFromMergedQc
#'
#' @param params
#' @param data
#'
#' @return
#' @export
updateTsFromMergedQc <- function(params,data){
  assignVariablesFromList(params)
  assignVariablesFromList(data)
  #5. Update sTS from merged qc
  nsTS <- updateFromQC(sTS,qc,project,projVersion)

  nTS <- combineInterpretationByScope(nsTS)


  if(standardizeTerms){#To do: #make this its own function
    #proxy lumps
    groupFrom <- c("paleoData_proxy","paleoData_inferredMaterial","interpretation1_variable","interpretation2_variable","interpretation3_variable","interpretation4_variable","interpretation5_variable","interpretation6_variable","interpretation7_variable","interpretation8_variable")

    groupInto <- c("paleoData_proxyLumps","paleoData_inferredMaterialGroup","interpretation1_variableGroup","interpretation2_variableGroup","interpretation3_variableGroup","interpretation4_variableGroup","interpretation5_variableGroup","interpretation6_variableGroup","interpretation7_variableGroup","interpretation8_variableGroup")

    #create new vectors for grouping variables.
    nTS <- createVectorsForGroups(nTS,groupFrom,groupInto)

    #Do some cleaning
    nTS <- standardizeTsValues(nTS)



    #add directions to isotope groups
    igf <- c("interpretation1_variableGroup","interpretation2_variableGroup","interpretation3_variableGroup","interpretation4_variableGroup","interpretation5_variableGroup","interpretation6_variableGroup","interpretation7_variableGroup","interpretation8_variableGroup")
    igt <- c("interpretation1_variableGroupDirection","interpretation2_variableGroupDirection","interpretation3_variableGroupDirection","interpretation4_variableGroupDirection","interpretation5_variableGroupDirection","interpretation6_variableGroupDirection","interpretation7_variableGroupDirection","interpretation8_variableGroupDirection")
    nTS <- createInterpretationGroupDirections(nTS,igf,igt)

    nTS <- fix_pubYear(nTS)
    nTS <- fixKiloyearsTs(nTS)
    nTS <- purrr::map(nTS,removeEmptyInterpretationsFromTs)
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

  #check to see which datasets are this compilation
  itc <- inThisCompilation(nTS,project,projVersion)
  ndsn <- pullTsVariable(nTS, "dataSetName")
  dsnInComp <- ndsn[map_lgl(itc,isTRUE)]
  dsnNotInComp <- ndsn[!map_lgl(itc,isTRUE)]
  nicdi <- which(!names(nD) %in% dsnInComp)



  # update file and project changelogs
  #first file changelogs
  dsidsNew <- tibble(datasetId = map_chr(nD,"datasetId"),
                     dataSetNameNew = map_chr(nD,"dataSetName"),
                     dataSetVersion = purrr::map_chr(nD,getVersion))


  #figure out change notes

  dsidKey <- dplyr::left_join(dsidsNew,dsidsOriginal,by = "datasetId")


  #loop through DSid and create changelog (this is for files, not for the project)
  for(dfi in 1:nrow(dsidKey)){
    newName <- dsidKey$dataSetNameNew[dfi]
    oldName <- dsidKey$dataSetNameOrig[dfi]

    cl <- createChangelog(Dloaded[[oldName]],nD[[newName]])
    nD[[newName]] <- updateChangelog(nD[[newName]],
                                     changelog = cl,
                                     notes = dsidKey$changes[dfi])
  }


  newData <- list(nD = nD,
                  nTS = nTS,
                  ndsn = ndsn,
                  nicdi = nicdi,
                  dsidKey = dsidKey,
                  dsnInComp = dsnInComp)


  data <- append(data,newData)
  return(data)


}

createDataPages <- function(params,data){
  assignVariablesFromList(params)
  assignVariablesFromList(data)

  #temporary
  #create changelog
  for(d in 1:length(nD)){
    if(is.null(nD[[d]]$changelog)){
      nD[[d]] <- initializeChangelog(nD[[d]])
    }
  }

  googlesheets4::gs4_auth(email = googEmail,cache = ".secret")
  newInv <- createInventory(nD)
  oldInv <- getInventory(lipdDir,googEmail)

  #find any updates to versions, or new datasets that we need to create for this
  if(recreateDataPages){

    toCreate <- dplyr::full_join(oldInv,newInv,by = "datasetId")
    toUpdate <- data.frame()
  }else{#only create what's changed





    toCreate <- dplyr::full_join(oldInv,newInv,by = "datasetId") %>%
      dplyr::filter(dataSetVersion.x != dataSetVersion.y |  is.na(dataSetVersion.x))


    #update pages for data in compilation, but that didn't change

    toUpdate <- dplyr::full_join(oldInv,newInv,by = "datasetId") %>%
      dplyr::filter(dataSetVersion.x == dataSetVersion.y &  !is.na(dataSetVersion.x))

  }

  if(nrow(toUpdate) > 0 & nrow(toCreate) > 0){#check to make sure were good, if need be
    #make sure distinct from create
    if(any(toCreate$datasetId %in% toUpdate$datasetId)){
      stop("Data pages to create and update are not distinct (and they should be)")
    }
  }


  if(nrow(toCreate) > 0){
    #create new datapages for the appropriate files
    tc <- nD[toCreate$dataSetNameNew.y]
    purrr::walk(tc,createDataWebPage,webdir = webDirectory)
  }

  #if  changes
  if(nrow(toUpdate) > 0){
    #create new datapages for the appropriate files
    tu <- nD[toUpdate$dataSetNameNew.y]
    purrr::walk(tu,updateDataWebPageForCompilation,webdir = webDirectory)
  }




  #pass on to the next
  newData <- list(newInv = newInv,
                  oldInv = oldInv,
                  toCreate = toCreate)
  data <- append(data,newData)
  return(data)

}



#' Create lipdverse pages for this version of the project
#'
#' @param params
#' @param data
#'
#' @return
#' @export
createProjectWebpages <- function(params,data){
  assignVariablesFromList(params)
  assignVariablesFromList(data)


  #create this version overview page
  createProjectSidebarHtml(project, projVersion,webDirectory)
  createProjectOverviewPage(project,projVersion,webDirectory)

  #update lipdverse overview page
  createProjectSidebarHtml("lipdverse", "current_version",webDirectory)
  createProjectOverviewPage("lipdverse", "current_version",webDirectory)

  #update data QC sheet
  updateQueryCsv(nD)

  #get only those in the compilation
  nDic <- nD[unique(dsnInComp)]

  tcdf <- data.frame(dsid = map_chr(nDic,"datasetId"),
                     dsn = map_chr(nDic,"dataSetName"),
                     vers = map_chr(nDic,getVersion))

  #create all the project shell sites

  purrr::pwalk(tcdf,
               createProjectDataWebPage,
               webdir = webDirectory,
               project,
               projVersion)

  #create a project map
  itc <- inThisCompilation(nTS,project,projVersion)
  nnTS <- nTS[which(itc)]
  createProjectMapHtml(nnTS,project = project,projVersion = projVersion,webdir = webDirectory)


  #get lipdverse inventory
  allDataDir <- list.dirs("~/Dropbox/lipdverse/html/data/",recursive = FALSE)

  getDataDetails <- function(datadir){
    maxVers <- list.dirs(datadir)[-1] %>%
      basename() %>%
      stringr::str_replace_all(pattern = "_",replacement = ".") %>%
      as.numeric_version() %>%
      max() %>%
      as.character() %>%
      stringr::str_replace_all(pattern = "[.]",replacement = "_")

    dsid <- datadir %>% basename()

    fnames <- list.files(file.path(datadir,maxVers))
    fnamesFull <- list.files(file.path(datadir,maxVers),full.names = TRUE)

    dsni <- fnames %>%
      stringr::str_detect(pattern = ".lpd") %>%
      which()

    longest <- dsni[which.max(purrr::map_dbl(fnames[dsni],stringr::str_length))]

    dsn <- fnames[longest] %>% stringr::str_remove(pattern = ".lpd")

    path <- fnamesFull[longest]

    return(data.frame(
      dsid = dsid,
      dsn = dsn,
      vers = stringr::str_replace_all(string = maxVers,pattern = "_",replacement = "."),
      path = path))


  }



  #sure that data files exist for all of the data in the database

  lipdverseDirectory <- purrr:::map_dfr(allDataDir,getDataDetails)

  LV <- readLipd(lipdverseDirectory$path)

  allDataDetails <- data.frame(dsid = map_chr(LV,"datasetId"),
                               dsn = map_chr(LV,"dataSetName"),
                               vers = map_chr(LV,getVersion))



  add <- dplyr::left_join(allDataDetails,lipdverseDirectory,by = "dsid")

  lvtc <- function(versO,versN){
    versO[is.na(versO)] <- "0.0.0"
    versN[is.na(versN)] <- "0.0.0"
    return(as.numeric_version(versO) > as.numeric_version(versN))
  }

  whichUpdated <- which(lvtc(add$vers.x,add$vers.y))

  if(length(whichUpdated) > 0){
    dsnu <- nD[add$dsn.x[whichUpdated]]


    walk(dsnu,createDataWebPage,webdir = webDirectory)
    #create lipdverse project pages
  }


  #find missing lipdverse htmls
  lpht <- list.files("~/Dropbox/lipdverse/html/lipdverse/current_version/",pattern = ".html")
  lphtdsn <- stringr::str_remove_all(lpht,pattern = ".html")

  addh <- which(!allDataDetails$dsn %in% lphtdsn)

  if(length(addh) > 0){
    lphtdf <- allDataDetails[addh,]

    #create all the project shell sites

    purrr::pwalk(lphtdf,
                 createProjectDataWebPage,
                 webdir = webDirectory,
                 project = "lipdverse",
                 projVersion = "current_version")

  }


  #look for updated lipdverse htmls

  lphtfull <- list.files("~/Dropbox/lipdverse/html/lipdverse/current_version/",pattern = ".html",full.names = TRUE)

  lpht <- list.files("~/Dropbox/lipdverse/html/lipdverse/current_version/",pattern = ".html",full.names = FALSE)

  getLipdverseHtmlVersions <- function(lfile){
    lss <- readLines(lfile)
    sbl <- max(which(stringr::str_detect(lss,"sidebar.html")))
    vers <- as.character(stringr::str_match_all(lss[sbl],"[0-9]*_[0-9]*_[0-9]*")[[1]])
    vers <- str_replace_all(vers,"_",".")
    return(vers)
  }

  lphtdsn <- stringr::str_remove_all(lpht,pattern = ".html")


  htmlVers <- map_chr(lphtfull,getLipdverseHtmlVersions)

  addv <- dplyr::left_join(allDataDetails,data.frame(dsn = lphtdsn,vers = htmlVers),by = "dsn")

  whichUpdatedHtml <- which(lvtc(addv$vers.x,addv$vers.y))

  if(length(whichUpdatedHtml) > 0){
    lphtdf <- allDataDetails[whichUpdatedHtml,]

    #create all the project shell sites

    purrr::pwalk(lphtdf,
                 createProjectDataWebPage,
                 webdir = webDirectory,
                 project = "lipdverse",
                 projVersion = "current_version")

  }
  #lipdverse htmls to remove

  # #don't do this for now, because it doesn't work with multiple data directories
  # todeht <- which(!lphtdsn %in% allDataDetails$dsn)
  # lphtdsn[todeht]

  #update lipdverse map
  LVTS <- extractTs(LV)

  createProjectMapHtml(LVTS,project = "lipdverse",projVersion = "current_version",webdir = webDirectory)


  #create lipdverse querying csv



  #reassign
  DF <- nDic

  if(serialize){
    try(createSerializations(D = DF,webDirectory,project,projVersion),silent = TRUE)
    try(createSerializations(D = LV,webDirectory,"lipdverse","current_version"),silent = TRUE)

  }

  #add datasets not in compilation into DF
  if(length(nicdi)>0){
    DF <- append(DF,nD[nicdi])
  }

  if(length(DF) != length(nD)){
    stop("Uh oh, you lost or gained datasets while creating the webpages")
  }


  TSF <- extractTs(DF)
  #get most recent in compilations
  mics <- getMostRecentInCompilationsTs(TSF)
  TSF <- pushTsVariable(TSF,variable = "paleoData_mostRecentCompilations",vec = mics,createNew = TRUE)
  sTSF <- splitInterpretationByScope(TSF)
  qcF <- createQCdataFrame(sTSF,templateId = qcId,ageOrYear = ageOrYear,compilationName = project,compVersion = projVersion)

  newData <- list(qcF = qcF,
                  DF = DF)

  data$nD <- NULL
  data$nTS <- NULL

  return(append(data,newData))

}



#' Create lipdversePages old framework
#'
#' @param params
#' @param data
#'
#' @return
#' @export
createWebpages <- function(params,data){
  assignVariablesFromList(params)
  assignVariablesFromList(data)

  #6 Update lipdverse
  if(updateWebpages){
    #restrict as necessary
    if(restrictWebpagesToCompilation){
      ictsi <- which(ndsn %in% dsnInComp)
      icdi <- which(names(nD) %in% dsnInComp)
      if(length(ictsi) == 0 || length(icdi) == 0){
        stop("didn't find any datasets in the compilation for the webpage")
      }
    }else{
      ictsi <- seq_along(nTS)
      icdi <- seq_along(nD)
      nicdi <- NULL
    }


    createProjectDashboards(nD[icdi],nTS[ictsi],webDirectory,project,projVersion)

    #load back in files
    DF <- readLipd(file.path(webDirectory,project,projVersion))

    if(serialize){
      try(createSerializations(D = DF,webDirectory,project,projVersion),silent = TRUE)
    }

    #add datasets not in compilation into DF
    if(length(nicdi)>0){
      DF <- append(DF,nD[nicdi])
    }

    if(length(DF) != length(nD)){
      stop("Uh oh, you lost or gained datasets while creating the webpages")
    }

  }else{
    DF <- nD
  }
  TSF <- extractTs(DF)
  #get most recent in compilations
  mics <- getMostRecentInCompilationsTs(TSF)
  TSF <- pushTsVariable(TSF,variable = "paleoData_mostRecentCompilations",vec = mics,createNew = TRUE)
  sTSF <- splitInterpretationByScope(TSF)
  qcF <- createQCdataFrame(sTSF,templateId = qcId,ageOrYear = ageOrYear,compilationName = project,compVersion = projVersion)

  newData <- list(TSF = TSF,
                  sTSF = sTSF,
                  qcF = qcF,
                  DF = DF)

  return(append(data,newData))

}


#' Update google
#'
#' @param params
#' @param data
#'
#' @return
#' @export
updateGoogleQc <- function(params,data){
  assignVariablesFromList(params)
  assignVariablesFromList(data)
  #7 Update QC sheet on google (and make a lastUpdate.csv file)

  qc2w <- qcF
  qc2w[is.null(qc2w) | qc2w == ""] <- NA


  #find differences for log
  #diff <- daff::diff_data(qcA,qc2w,ids = "TSid",ignore_whitespace = TRUE,columns_to_ignore = "link to lipdverse",never_show_order = TRUE)

  qc2w[is.na(qc2w)] <- ""


  # goodDatasets <- unique(qc2w$dataSetName[which(qc2w$inThisCompilation == "TRUE")])
  #
  # gi <- which(qc2w$dataSetName %in% goodDatasets)
  # qc2w <- qc2w[gi,]

  #update the data compilation page
  updateDatasetCompilationQc(DF,project,projVersion,qcId)

  googlesheets4::gs4_auth(email = googEmail,cache = ".secret")
  googlesheets4::write_sheet(qc2w,ss = lastUpdateId,sheet = 1)
  googlesheets4::write_sheet(qc2w,ss = qcId, sheet = 1)
  googledrive::drive_rename(googledrive::as_id(qcId),name = stringr::str_c(project," v.",projVersion," QC sheet"))
  readr::write_csv(qc2w,path = file.path(webDirectory,project,"newLastUpdate.csv"))


  #daff::render_diff(diff,file = file.path(webDirectory,project,projVersion,"metadataChangelog.html"),title = paste("Metadata changelog:",project,projVersion),view = FALSE)

  #googledrive::drive_update(file = googledrive::as_id(lastUpdateId),media = file.path(webDirectory,project,"newLastUpdate.csv"))


  #newName <- stringr::str_c(project," v.",projVersion," QC sheet")


  #googledrive::drive_update(file = googledrive::as_id(qcId),media = file.path(webDirectory,project,"newLastUpdate.csv"),name = newName)


  #remove unneeded data
  neededVariablesMovingForward <-  c("dsidKey",
                                     "webDirectory",
                                     "project",
                                     "lastVersionNumber",
                                     "DF",
                                     "projVersion",
                                     "webDirectory",
                                     "googEmail",
                                     "versionMetaId",
                                     "filesToUltimatelyDelete",
                                     "lipdDir")

  vToRemove <- names(data)[!names(data) %in% neededVariablesMovingForward]

  for(v2r in vToRemove){
    data[v2r] <- NULL
  }


  return(data)

}

#' Finalize
#'
#' @param params
#' @param data
#'
#' @return
#' @export
finalize <- function(params,data){

  assignVariablesFromList(params)
  assignVariablesFromList(data)

  #8 finalize and write lipd files

  #DF <- purrr::map(DF,removeEmptyPubs)



  #9 update the google version file
  versionDf <- googlesheets4::read_sheet(googledrive::as_id(versionMetaId),col_types = "cdddccccc")
  #versionDf <- googlesheets4::read_sheet(googledrive::as_id(versionMetaId))
  versionDf$versionCreated <- lubridate::ymd_hms(versionDf$versionCreated)


  newRow <- versionDf[1,]

  newRow$project <- project
  pdm <- as.numeric(unlist(str_split(projVersion,"_")))
  newRow$publication <- pdm[1]
  newRow$dataset <- pdm[2]
  newRow$metadata <- pdm[3]
  newRow$dsns <- paste(unique(dsnInComp),collapse = "|")
  newRow$versionCreated <-  lubridate::ymd_hms(lubridate::now(tzone = "UTC"))
  newRow$`zip MD5` <- directoryMD5(lipdDir)

  #check for differences in dsns
  dsndiff <- filter(versionDf,project == (!!project)) %>%
    filter(versionCreated == max(versionCreated,na.rm = TRUE))

  lastVersionNumber <- paste(dsndiff[1,2:4],collapse = "_")

  oldDsns <- stringr::str_split(dsndiff$dsns,pattern = "[|]",simplify = T)
  newDsns <- stringr::str_split(newRow$dsns,pattern = "[|]",simplify = T)

  newRow$`dataSets removed` <- paste(setdiff(oldDsns,newDsns),collapse = "|")
  newRow$`dataSets added` <- paste(setdiff(newDsns,oldDsns),collapse = "|")

  nvdf <- dplyr::bind_rows(versionDf,newRow)

  nvdf$versionCreated <- as.character(nvdf$versionCreated)

  readr::write_csv(nvdf,file = file.path(tempdir(),"versTemp.csv"))


  data$lastVersionNumber <- lastVersionNumber


  #remove unneeded data
  neededVariablesMovingForward <-  c("dsidKey",
                                     "webDirectory",
                                     "project",
                                     "lastVersionNumber",
                                     "DF",
                                     "projVersion",
                                     "webDirectory",
                                     "googEmail",
                                     "versionMetaId",
                                     "filesToUltimatelyDelete",
                                     "lipdDir")

  vToRemove <- names(data)[!names(data) %in% neededVariablesMovingForward]

  for(v2r in vToRemove){
    data[v2r] <- NULL
  }




  return(data)
}

#' Log changes and update
#'
#' @param params
#' @param data
#'
#' @return
#' @export
changeloggingAndUpdating <- function(params,data){

  assignVariablesFromList(params)
  assignVariablesFromList(data)

  #write project changelog
  #get last project's data. Try serialiation first:
  lastSerial <- try(load(file.path(webDirectory,project,lastVersionNumber,paste0(project,lastVersionNumber,".RData"))),silent = TRUE)

  if(!class(lastSerial) == "try-error"){
    Dpo <- D
  }else{#try to load from lipd
    Dpo <- readLipd(file.path(webDirectory,project,lastVersionNumber))
  }


  if(length(Dpo)>0){
    createProjectChangelog(Dold = Dpo,
                           Dnew = DF,
                           proj = project,
                           projVersOld = lastVersionNumber,
                           projVersNew = projVersion,
                           webDirectory = webDirectory,
                           notesTib = dsidKey)

  }else{#write empty changelog
    cle <- glue::glue("## Changelog is empty - probably because there were no files in the web directory for {project} version {lastVersionNumber}")
    readr::write_file(cle,file.path(webDirectory,project,projVersion,"changelogEmpty.Rmd"))
    rmarkdown::render(file.path(webDirectory,project,projVersion,"changelogEmpty.Rmd"),
                      output_file = file.path(webDirectory,project,projVersion,"changelogSummary.html"))
    rmarkdown::render(file.path(webDirectory,project,projVersion,"changelogEmpty.Rmd"),
                      output_file = file.path(webDirectory,project,projVersion,"changelogDetail.html"))
  }


  vt <- readr::read_csv(file.path(tempdir(),"versTemp.csv"),col_types = "cdddccccc")

  googlesheets4::gs4_auth(email = googEmail,cache = ".secret")
  wrote <- try(googlesheets4::sheet_write(vt,ss = versionMetaId,sheet = 1))
  if(!class(wrote) == "try-error"){
    print("failed to write lipdverse versioning - do this manually")
  }
  #update datasetId information
  updateDatasetIdDereferencer(DF,
                              compilation = project,
                              version = projVersion,
                              dateUpdated = lubridate::today())

  #give permissions back
  #drive_share(as_id(qcId),role = "writer", type = "user",emailAddress = "")
  #update the files
  unlink(file.path(webDirectory,project,"current_version"),force = TRUE,recursive = TRUE)

  dir.create(file.path(webDirectory,project,"current_version"))

  file.copy(file.path(webDirectory,project,projVersion,.Platform$file.sep), file.path(webDirectory,project,"current_version",.Platform$file.sep), recursive=TRUE,overwrite = TRUE)

  file.copy(file.path(webDirectory,project,projVersion,str_c(project,projVersion,".zip")),
            file.path(webDirectory,project,"current_version","current_version.zip"),overwrite = TRUE)


  unlink(x = filesToUltimatelyDelete,force = TRUE, recursive = TRUE)
  writeLipd(DF,path = lipdDir,removeNamesFromLists = TRUE)

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
createSerializations <- function(D,
                                 webDirectory,
                                 project,
                                 projVersion,
                                 remove.ensembles = TRUE,
                                 matlabUtilitiesPath = "/Volumes/data/GitHub/LiPD-utilities/Matlab",
                                 matlabPath = "/Applications/MATLAB_R2021b.app/bin/matlab",
                                 python3Path="/Users/nicholas/opt/anaconda3/envs/pyleo/bin/python3"){
  #create serializations for web
  #R
  if(remove.ensembles){
    Do <- D
    D <- purrr::map(D,removeEnsembles)
  }

  if(object.size(Do) > object.size(D)){
    has.ensembles <- TRUE
  }else{
    has.ensembles <- FALSE
  }

  TS <- extractTs(D)
  sTS <- splitInterpretationByScope(TS)
  save(list = c("D","TS","sTS"),file = file.path(webDirectory,project,projVersion,stringr::str_c(project,projVersion,".RData")))



  #write files to a temporary directory
  lpdtmp <- file.path(tempdir(),"lpdTempSerialization")
  unlink(lpdtmp,recursive = TRUE)
  dir.create(lpdtmp)

  writeLipd(D,path = lpdtmp)

  #zip it
  zip(zipfile = file.path(webDirectory,project,projVersion,str_c(project,projVersion,".zip")),files = list.files(lpdtmp,pattern= "*.lpd",full.names = TRUE),extras = '-j')



  if(hasEnsembles){
    print("writing again with ensembles")
    TS <- extractTs(Do)
    sTS <- splitInterpretationByScope(TSo)
    save(list = c("D","TS","sTS"),file = file.path(webDirectory,project,projVersion,stringr::str_c(project,projVersion,"-ensembles.RData")))

    #write files to a temporary directory
    lpdtmpens <- file.path(tempdir(),"lpdTempSerializationEnsembles")
    unlink(lpdtmpens,recursive = TRUE)
    dir.create(lpdtmpens)

    writeLipd(Do,path = lpdtmpens)

    #zip it
    zip(zipfile = file.path(webDirectory,project,projVersion,str_c(project,projVersion,"-ensembles.zip")),files = list.files(lpdtmpens,pattern= "*.lpd",full.names = TRUE))
  }




  #matlab
  mfile <- stringr::str_c("addpath(genpath('",matlabUtilitiesPath,"'));\n") %>%
    stringr::str_c("D = readLiPD('",lpdtmp,"');\n") %>%
    stringr::str_c("TS = extractTs(D);\n") %>%
    stringr::str_c("sTS = splitInterpretationByScope(TS);\n") %>%
    stringr::str_c("save ",file.path(webDirectory,project,projVersion,stringr::str_c(project,projVersion,".mat")),' D TS sTS\n') %>%
    stringr::str_c("exit")

  #write the file
  readr::write_file(mfile,path = file.path(webDirectory,project,projVersion,"createSerialization.m"))

  #run the file
  try(system(stringr::str_c(matlabPath," -nodesktop -nosplash -nodisplay -r \"run('",file.path(webDirectory,project,projVersion,"createSerialization.m"),"')\"")))


  #Python
  pyfile <- "import lipd\n" %>%
    stringr::str_c("import pickle\n") %>%
    stringr::str_c("D = lipd.readLipd('",lpdtmp,"/')\n") %>%
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
  try(system(stringr::str_c(python3Path, " ",file.path(webDirectory,project,projVersion,"createSerialization.py"))))

}


