.libPaths("/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/MacOS/pandoc")
library(lipdverseR)
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
                       updateWebpages = FALSE,
                       standardizeTerms = FALSE,
                       serialize = FALSE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2 = getQcInfo(params,data1),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data6 = createWebpages(params,data5),
  data7 = updateGoogleQc(params,data6),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
  )

test <- drake_plan(
  params = buildParams("test",
                       "/Users/nicholas/Dropbox/lipdverse/testDatabase",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1P0_e-frsQIYFLjLBiJTfouEbdIhMW7UPfYGZBnaDep0",
                       lastUpdateId = "1RbAs0qRWqvHCUfI7q_Er5UKAxRy-otRh7pdM2PKYCHw",
                       googEmail = "nick.mckay2@gmail.com",
                       updateWebpages = TRUE,
                       standardizeTerms = FALSE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2 = getQcInfo(params,data1),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data6 = createWebpages(params,data5),
  data7 = updateGoogleQc(params,data6),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)

test <- drake_plan(
  params = buildParams("test",
                       "/Users/nicholas/Dropbox/lipdverse/testDatabase",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1P0_e-frsQIYFLjLBiJTfouEbdIhMW7UPfYGZBnaDep0",
                       lastUpdateId = "1RbAs0qRWqvHCUfI7q_Er5UKAxRy-otRh7pdM2PKYCHw",
                       googEmail = "nick.mckay2@gmail.com",
                       updateWebpages = TRUE,
                       standardizeTerms = FALSE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2 = getQcInfo(params,data1),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data6 = createWebpages(params,data5),
  data7 = updateGoogleQc(params,data6),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)


#run it
drake::make(HoloceneHydroclimate,lock_envir = FALSE)