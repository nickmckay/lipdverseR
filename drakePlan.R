.libPaths("/Library/Frameworks/R.framework/Versions/4.1/Resources/library")
rmarkdown::find_pandoc(dir = "/Applications/RStudio.app/Contents/MacOS/quarto/bin/tools")


#library(lipdverseR)
library(magrittr)
library(tidyverse)
library(lipdR)
library(glue)
library(drake)
setwd("/Users/nicholas/GitHub/lipdverseR/")

googEmail <- "nick.mckay2@gmail.com"
#authorize google
googlesheets4::gs4_auth(email = googEmail,cache = ".secret")
googledrive::drive_auth(email = googEmail,cache = ".secret")


HoloceneHydroclimate <- drake_plan(
  params = buildParams("HoloceneHydroclimate",
                       "/Users/nicholas/Dropbox/lipdverse/database",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1rhYoL0B5OfE5A-rNwuQZfnmjI3Vj3NCX07r-Mif3Ncs",
                       lastUpdateId = "1gny_fbwLZRchkPZyh_GPUoCtXDvMHWDpozGXg2u07hA",
                       googEmail = "nick.mckay2@gmail.com",
                       updateWebpages = TRUE,
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2 = getQcInfo(params,data1),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
  )

test <- drake_plan(
  params = buildParams("test",
                       "/Users/nicholas/Dropbox/lipdverse/testDatabase",
                       "/Users/nicholas/Dropbox/lipdverse/htmlTest/",
                       qcId = "1P0_e-frsQIYFLjLBiJTfouEbdIhMW7UPfYGZBnaDep0",
                       lastUpdateId = "1RbAs0qRWqvHCUfI7q_Er5UKAxRy-otRh7pdM2PKYCHw",
                       googEmail = "nick.mckay2@gmail.com",
                       recreateDataPages = TRUE,
                       updateWebpages = TRUE,
                       serialize = TRUE,
                       standardizeTerms = FALSE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2 = getQcInfo(params,data1),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)

RAW <- drake_plan(
  params = buildParams("RapidArcticWarming",
                       "/Volumes/data/Dropbox/lipdverse/database",
                       "/Volumes/data/Dropbox/lipdverse/html/",
                       qcId = "1tYuhgaDPx1HxdSneL0Nl1Aq7LIM14jzbn5Ke55ha_z0",
                       lastUpdateId = "1OEUGZrqo5Ipz8lBZy9hvxtOeOPOc38sswv3-laFBobU",
                       googEmail = "nick.mckay2@gmail.com",
                       updateWebpages = TRUE,
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2 = getQcInfo(params,data1),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)


CH2k <- drake_plan(
  params = buildParams("CoralHydro2k",
                       "/Volumes/data/Dropbox/lipdverse/CoralHydro2k/",
                       "/Volumes/data/Dropbox/lipdverse/html/",
                       qcId = "1FJAZrPlqc8rYT7cb1sMwEHnpyMSGcQ96MFGsKYJydgU",
                       lastUpdateId = "1N1F1pmepvW3r0l8Wqqm0ibp7MmNJnwAml-quPDQ1wzs",
                       googEmail = "nick.mckay2@gmail.com",
                      #projVersion ="0_5_0",
                       updateWebpages = TRUE,
                       ageOrYear = "year",
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2 = getQcInfo(params,data1),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)

Temp12k <- drake_plan(
  params = buildParams("Temp12k",
                       "/Volumes/data/Dropbox/lipdverse/database/",
                       "/Volumes/data/Dropbox/lipdverse/html/",
                       qcId = "1JEm791Nhd4fUuyqece51CSlbR2A2I-pf8B0kFgwynug",
                       lastUpdateId = "1qLRMCfDMbTyffJBWlIj3Zw4CAhJY2SECIY-ckcZ2Wak",
                       googEmail = "nick.mckay2@gmail.com",
                       updateWebpages = TRUE,
                       ageOrYear = "age",
                       standardizeTerms = TRUE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2 = getQcInfo(params,data1),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)

iso2k <- drake_plan(
  params = buildParams("iso2k",
                       "/Users/nicholas/Dropbox/lipdverse/iso2k/",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1jdSQqUkR_YuJO8Hsp2x3hOhrW19a0SL6wB2mbp4DA2M",
                       lastUpdateId = "1m0-obq3etFf8fvcfN-jrxsYOHqDV7lcsMpZmidxZKis",
                       googEmail = "nick.mckay2@gmail.com",
                       recreateDataPages = FALSE,
                       updateWebpages = TRUE,
                       ageOrYear = "year",
                       serialize = TRUE,
                       standardizeTerms = FALSE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2 = getQcInfo(params,data1),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)

pages2k <- drake_plan(
  params = buildParams("Pages2kTemperature",
                       "/Users/nicholas/Dropbox/lipdverse/Pages2kTemperature/",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1_ZvQXV-jXMLi7DSC9vc8tAXdlimfkYrna0fKbIB_5Og",
                       lastUpdateId = "11Vh1iCxt0bEq4a8jlM3uawiPX-C8un8i8GUji3S8guA",
                       googEmail = "nick.mckay2@gmail.com",
                       recreateDataPages = FALSE,
                       updateWebpages = TRUE,
                       ageOrYear = "year",
                       serialize = TRUE,
                       standardizeTerms = FALSE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2 = getQcInfo(params,data1),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)

#run it
count <- 19
while(TRUE){
  count <- count+1
  if(count > 20){
    break
  }
  print(paste("try",count))
  try(
    drake::make(RAW,lock_envir = FALSE)
  )
  prog <- drake::drake_progress()
  if(all(prog$progress == "done")){
    break
  }
}

af <- list.dirs("~/Dropbox/lipdverse/html/",recursive = FALSE,full.names = FALSE)

#rsync -rvauz --delete /Users/nicholas/Dropbox/lipdverse/html/data/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/data

  #rsync -rvauz --delete /Users/nicholas/Dropbox/lipdverse/html/data/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/data

#rsync -rvauz --delete /Users/nicholas/Dropbox/lipdverse/html/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse
