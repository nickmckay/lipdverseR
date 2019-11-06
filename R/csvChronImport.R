# #update chronology from csvs
# library(tidyverse)
# library(lipdR)
# library(geoChronR)
#
#
# duplicateTSid <- c()
# noTSidMatch <- c()
# noTSid <- c()
# missingVarName <- c()
#
# csvDir <- "/Users/npm4/Dropbox/HoloceneLiPDLibrary/2. chronCsvs/Revised and LiPD'ed/"
# lpdDir <- "~/Dropbox/HoloceneLiPDLibrary/2. chronCsvs/masterDatabaseOld/"
# outDir <- "~/Dropbox/HoloceneLiPDLibrary/importedChron/"
#
# #1. read in csv
# csvs <- list.files(csvDir,pattern = "*.csv")
#
#
#
# for(cc in 1:length(csvs)){
# csv <- csvs[cc]
#   #lipd name
#   dsn <- str_c(str_remove(csv,pattern = "-chron.csv"),".lpd")
#
#
#   #read in components
#   new <- read_csv(file = paste0(csvDir,csv),col_names = FALSE)
#   colNames <- new[1,]
#   cTSids <- t(as.matrix(new[2,]))
#   newData <- read_csv(file = paste0(csvDir,csv),col_names = FALSE,skip = 2)
#
#   #clean names
#   colNames <- str_replace(string = colNames,pattern = "\xb1 ","")
#   varNames <- str_trim(str_extract(colNames,pattern = "^[^(]+"))
#   if(any(is.na(varNames))){
#     stop("bad variable names in csv")
#   }
#
#   units <- str_extract(colNames,pattern = "(?<=\\().*?(?=\\))")
#   units[units == ""] <- "unitless"
#
#   #any to delete?
#   td <- grepl(colNames,pattern = "delete",ignore.case = TRUE)
#
#
#
#
#   #2. read in lipds
#   L <- readLipd(str_c(lpdDir,dsn))
#
#   cts <- extractTs(L,whichtables = "meas",mode = "chron")
#   newTd <- matrix(FALSE, nrow = length(cts))
#
#
#   tsid <- try(pullTsVariable(cts,"chronData_TSid"))
#   if(class(tsid)=="try-error"){
#     noTSid <- c(noTSid,dsn)
#     tsid <- purrr::map_chr(.x = 1:length(cts),lipdR::createTSid)
#     newTd <- matrix(TRUE, nrow = length(cts))
#   }
#
#   #3 loop through TSids
#   for(i in 1:length(cTSids)){
#     addColumn <- FALSE
#     wi <- which(cTSids[i]==tsid)
#     if(length(wi)>1){
#       duplicateTSid <- c(duplicateTSid,str_c(dsn,": ",as.character(cTSids[i])))
#       warning(str_c(dsn,": ",cTSids[i],": duplicate TSids"))
#       next
#     }else if(length(wi)==0){
#       if(is.na(cTSids[i])){
#         noTSidMatch <- c(noTSidMatch,dsn)
#       }else{
#         if(grepl("add",cTSids[i],ignore.case = TRUE)){
#           addColumn <- TRUE
#         }else{
#           noTSidMatch <- c(noTSidMatch,str_c(dsn,": ",as.character(cTSids[i])))
#         }
#       }
#       if(!addColumn){
#         warning(str_c(dsn,": ",cTSids[i],": no matching TSid"))
#         next
#       }
#     }
#
#     if(!addColumn){
#     te <- cts[[wi]]
#     }else{#create a new column
#       te <- cts[[1]]
#       te$chronData_TSid <- createTSid()
#     }
#
#     #check name
#     if(is.na(varNames[i])){
#       missingVarName <- c(missingVarName,dsn)
#       next
#     }
#
#     if(te$chronData_variableName != varNames[i]){
#       #leave a note
#       te$chronData_QCNotes <- str_c("Variable name changed from ",te$chronData_variableName," to ",varNames[i]," during update from DKs csv files.")
#       #assign it in
#       te$chronData_variableName <-  varNames[i]
#     }
#
#     #assing in units and values
#     te$chronData_units <-  units[i]
#     te$chronData_values <- as.matrix(newData[,i])
#
#     #write back out.
#     if(addColumn){
#       cts[[length(cts)+1]] <- te
#     }else{
#       cts[[wi]] <- te
#     }
#     #figure out whether it should be deleted
#
#     if(addColumn){
#       newTd[length(cts)] <- FALSE
#     }else{
#       newTd[wi] <- td[i]
#     }
#   }
#
#   #4 delete rows that should be deleted
#   if(any(newTd)){
#     wtd <- which(newTd)
#     cts[wtd] <- NULL
#   }
#
#
#   L <- collapseTs(cts)
#   writeLipd(L,str_c(outDir,dsn))
# }
