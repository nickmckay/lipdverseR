createNewLipdverseProject <- function(){
#create new project
proj <- "Lakes380"
init.vers <- "0_1_0"
lipd.dir <- "/Users/nicholas/Dropbox/lipdverse/lakes380/"
web.dir <- "/Users/nicholas/Dropbox/lipdverse/html/"
versionMetaId <-  "1OHD7PXEQ_5Lq6GxtzYvPA76bpQvN1_eYoFR0X80FIrY"
googEmail <-  "nick.mckay2@gmail.com"
templateId <- "1Tjs-WjZdy5DxpIrI2Sd20VVQqCF4fy23-1HTN2zVinw"
restrictWebpagesToCompilation <- TRUE
initializeCompilationVersion <- TRUE
makeWebpages <- TRUE

# load in the data
D <- readLipd(lipd.dir)
TS <- extractTs(D)
sTS <- splitInterpretationByScope(TS)

#create the web directories
dir.create(file.path(web.dir,proj,init.vers),recursive = T)


if(initializeCompilationVersion){
  sTS <- pushTsVariable(sTS,"inCompilationBeta1_compilationName",matrix(proj,nrow = length(sTS)),createNew = TRUE)
  sTS <- pushTsVariable(sTS,"inCompilationBeta1_compilationVersion",matrix(init.vers,nrow = length(sTS)),createNew = TRUE)
  TS <- combineInterpretationByScope(sTS)
  D <- collapseTs(TS)
}

#write the Lipdverse pages
itc <- inThisCompilation(sTS,proj,init.vers)


ndsn <- pullTsVariable(sTS, "dataSetName")
dsnInComp <- ndsn[purrr::map_lgl(itc,isTRUE)]
dsnNotInComp <- ndsn[!purrr::map_lgl(itc,isTRUE)]
nicdi <- which(!names(D) %in% dsnInComp)

  #restrict as necessary
  if(restrictWebpagesToCompilation){
    ictsi <- which(ndsn %in% dsnInComp)
    icdi <- which(names(D) %in% dsnInComp)
    if(length(ictsi) == 0 || length(icdi) == 0){
      stop("didn't find any datasets in the compilation for the webpage")
    }
  }else{
    ictsi <- seq_along(nTS)
    icdi <- seq_along(nD)
    nicdi <- NULL
  }

if(makeWebpages){
  createProjectDashboards(D[icdi],TS[ictsi],web.dir,proj,init.vers)

  #load back in files
  DF <- readLipd(file.path(web.dir,proj,init.vers))

  #serializations
  createSerializations(D = DF,web.dir,proj,init.vers)


  #add datasets not in compilation into DF
  if(length(nicdi)>0){
    DF <- append(DF,nD[nicdi])
  }

  if(length(DF) != length(D)){
    stop("Uh oh, you lost or gained datasets while creating the webpages")
  }
}else{
  DF <- D
}
  nTS <- extractTs(DF)
  nsTS <- splitInterpretationByScope(nTS)


  #authorize google
  googlesheets4::sheets_auth(email = googEmail,cache = ".secret")
  googledrive::drive_auth(email = googEmail,cache = ".secret")

#create a qc sheet
  qcsheet <- createQCdataFrame(nsTS,templateId = "1Tjs-WjZdy5DxpIrI2Sd20VVQqCF4fy23-1HTN2zVinw",ageOrYear = "age",compilationName = proj,compVersion = init.vers)

#create a google qc sheet
createNewQCSheet(qcsheet,stringr::str_c(proj," v.",init.vers," QC sheet"))

#and last update
createNewQCSheet(qcsheet,stringr::str_c(proj,"-lastUpdate"))

#finally, add project to the versioning sheet

#9 update the google version file
versionDf <- googlesheets4::read_sheet(googledrive::as_id(versionMetaId))
versionDf$versionCreated <- lubridate::ymd_hms(versionDf$versionCreated)


newRow <- versionDf[1,]

newRow$project <- proj
pdm <- as.numeric(unlist(str_split(init.vers,"_")))
newRow$publication <- pdm[1]
newRow$dataset <- pdm[2]
newRow$metadata <- pdm[3]
newRow$dsns <- paste(unique(dsnInComp),collapse = "|")
newRow$versionCreated <- lubridate::now(tzone = "UTC")
newRow$`zip MD5` <- directoryMD5(lipd.dir)

nvdf <- dplyr::bind_rows(versionDf,newRow)
readr::write_csv(nvdf,path = file.path(tempdir(),"versTemp.csv"))
googledrive::drive_update(media = file.path(tempdir(),"versTemp.csv"),file = googledrive::as_id(versionMetaId),name = "lipdverse versioning spreadsheet")
}

