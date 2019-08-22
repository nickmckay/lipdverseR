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
# csvDir <- "~/Dropbox/HoloceneLiPDLibrary/1. Revised/"
# lpdDir <- "~/Dropbox/HoloceneLiPDLibrary/masterDatabase/"
# outDir <- "~/Dropbox/HoloceneLiPDLibrary/importedChron/"
#
# #1. read in csv
# csvs <- list.files(csvDir,pattern = "*.csv")
#
# for(cc in 328:length(csvs)){
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
#   varNames <- str_extract(colNames,pattern = "^[^ (]+")
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
#
#   tsid <- try(pullTsVariable(cts,"chronData_TSid"))
#   if(class(tsid)=="try-error"){
#     noTSid <- c(noTSid,dsn)
#     next
#   }
#
#   #3 loop through TSids
#   for(i in 1:length(cTSids)){
#     wi <- which(cTSids[i]==tsid)
#     if(length(wi)>1){
#       duplicateTSid <- c(duplicateTSid,str_c(dsn,": ",as.character(cTSids[i])))
#       warning(str_c(dsn,": ",cTSids[i],": duplicate TSids"))
#       next
#     }else if(length(wi)==0){
#       if(is.na(cTSids[i])){
#         noTSidMatch <- c(noTSidMatch,dsn)
#       }else{
#         noTSidMatch <- c(noTSidMatch,str_c(dsn,": ",as.character(cTSids[i])))
#       }
#       warning(str_c(dsn,": ",cTSids[i],": no matching TSid"))
#       next
#     }
#
#     te <- cts[[wi]]
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
#     cts[[wi]] <- te
#   }
#
#   #4 delete rows that should be deleted
#   if(any(td)){
#     wtd <- which(td)
#     cts[wtd] <- NULL
#   }
#
#
#   L <- collapseTs(cts)
#   writeLipd(L,str_c(outDir,dsn))
# }
