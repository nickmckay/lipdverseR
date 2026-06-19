dlp <- "~/Downloads/ 4. InLipdPendingLipdverse 2"

alp <- list.files(dlp, recursive = TRUE, pattern = ".lpd",full.names = TRUE)

N <- readLipd(alp)

set.seed(0223)#change for new batch
for(i in 1:length(N)){
  L <- N[[i]]
  if(is.null(L$datasetId)){
    L$datasetId <- createDatasetId()
  }
  if(is.null(L$changelog)){
    L <- lipdR::initializeChangelog(L)
  }
  N[[i]] <- fixPubAuthorList(L)
}

#problem files
bad <- which(!map_lgl(N,validLipd))
set.seed(1126)
# L <- N[[bad[1]]]
# L$paleoData[[1]]$measurementTable[[1]]$d13C$TSid <- lipdR::createTSid("h2k")
# L$paleoData[[1]]$measurementTable[[1]]$d18O$TSid <- lipdR::createTSid("h2k")
# L$paleoData[[1]]$measurementTable[[1]]$year$TSid <- lipdR::createTSid("h2k")
# L$paleoData[[1]]$measurementTable[[1]]$depth$TSid <- lipdR::createTSid("h2k")
# validLipd(L)
# N[[bad[1]]] <- L
#
# #load in recent files
#
#find files modified in the past hour
recentFiles <- function(filepaths = alp){
  ftimes <- file.info(filepaths)$mtime
  recentFiles <- filepaths[ftimes > (Sys.time() - 60*60)]
  return(recentFiles)
}

# rf <- recentFiles(list.files("~/Dropbox/lipdverse/database/",pattern = ".lpd",full.names = TRUE))
# N2 <- readLipd(rf)
updateVocabWebsites()
standardTables <- readRDS(url("https://lipdverse.org/lipdverse/standardTables.RDS"),"rb")
# now standardize files
CHS <- map(N,standardizeLipd,standardTables)

goodNow <- map_lgl(CHS,hasStandardizedVocabulary,standardTables,.progress = TRUE)
val <- map_lgl(CHS,validLipd)
all(val)







all(map(CHS,validLipd))
databaseRef <- createDatabaseReference(D)

set.seed(0223)
for(i in 1:length(CHS)){
  CHS[[i]]$datasetId <- createDatasetId()
}


for(i in 1:length(CHS)){
addLipdToDatabase(L = CHS[[i]],createdBy = "hydroclimate2k")
}


#update datasetsInCompilation to include all new files
rf <- recentFiles(list.files("~/Dropbox/lipdverse/database/",pattern = ".lpd",full.names = TRUE))
N2 <- readLipd(rf)
dsn <- purrr::map_chr(N2,"dataSetName")
dsid <- purrr::map_chr(N2,"datasetId")

dbir <- googlesheets4::read_sheet("1Bp8xw2NgMzvFBWtmVjC2y1Zf7RHXLRMQSQEzX3z5YI8",sheet = "datasetsInCompilation")

tbt <- which(dbir$dsn %in% dsn)
dbir$inComp[tbt] <- "TRUE"

#add new ones
ta <- which(!dsn %in% dbir$dsn)

nd <- data.frame(dsn = dsn[ta],dsid = dsid[ta],inComp = "TRUE",instructions = NA)
dbir <- dplyr::bind_rows(dbir,nd)

googlesheets4::write_sheet(dbir,ss = "1Bp8xw2NgMzvFBWtmVjC2y1Zf7RHXLRMQSQEzX3z5YI8",sheet = "datasetsInCompilation")




