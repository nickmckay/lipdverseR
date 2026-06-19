devtools::load_all(".")
#library(lipdverseR)
library(magrittr)
library(tidyverse)
library(lipdR)
library(glue)
library(drake)
#library(profvis)
setwd("/Users/nicholas/GitHub/lipdverseR/")

try(lipdR:::updateStandardTables())
#lipdR:::checkStandardTables()

googEmail <- "nick.mckay2@gmail.com"
#authorize google
googlesheets4::gs4_auth(email = googEmail,cache = ".secret")
googledrive::drive_auth(email = googEmail,cache = ".secret")


# HoloceneHydroclimate ----------------------------------------------------

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
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
  )


# hydroclimate2k -------------------------------------------------------------------------

hydroclimate2k <- drake_plan(
  params = buildParams("hydroclimate2k",
                       "/Users/nicholas/Dropbox/lipdverse/database",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1Bp8xw2NgMzvFBWtmVjC2y1Zf7RHXLRMQSQEzX3z5YI8",
                       lastUpdateId = "1xn76PBL4sdxiDO8KdTnSrjJac8mUFIkk3kssJpcIHx4",
                       googEmail = "nick.mckay2@gmail.com",
                        ageOrYear = "year",
                       updateWebpages = TRUE,
                       updateDatasetsInCompilationFromInThisCompilation = TRUE,
                       qcStandardizationCheck = FALSE,
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)



# FreeSoda ----------------------------------------------------------------


FreeSoda <- drake_plan(
  params = buildParams("FreeSoda",
                       "/Users/nicholas/Dropbox/lipdverse/database",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1pxQs_zTbNwZ50Wu0axlEZW0TzNcnOPcnDcV6JIIT3mc",
                       lastUpdateId = "13Ed1xLqQSbcGOkLqYOdIQOtxuJsuGQtAcDPVlG96dYs",
                       googEmail = "nick.mckay2@gmail.com",
                       ageOrYear = "age",
                       updateWebpages = TRUE,
                       updateDatasetsInCompilationFromInThisCompilation = TRUE,
                       qcStandardizationCheck = FALSE,
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)


# SISAL ----------------------------------------------------------------


SISAL <- drake_plan(
  params = buildParams("SISAL-LiPD",
                       "/Users/nicholas/Dropbox/lipdverse/database",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1clmzdTKcfOByGKF5lQVpmu4PQfIlqrpwIGVWhiWE2uk",
                       lastUpdateId = "1QB4eSFhAWp3F97p8Nymhf2OTUiLOulpfxkKpJUrGS0E",
                       googEmail = "nick.mckay2@gmail.com",
                       ageOrYear = "age",
                       updateWebpages = FALSE,
                       updateDatasetsInCompilationFromInThisCompilation = FALSE,
                       qcStandardizationCheck = FALSE,
                       standardizeTerms = FALSE,
                       serialize = FALSE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)

# Hydro21k ----------------------------------------------------------------


Hydro21k <- drake_plan(
  params = buildParams("Hydro21k",
                       "/Users/nicholas/Dropbox/lipdverse/database",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1k3PZTGZ1n1eljVbXx9qR-PtQ-7LIdj-mi7wtEmzu2iM",
                       lastUpdateId = "1uwoNny2tsul94zyD16txceRZLsGo1LEW3-1UcUX0GdY",
                       googEmail = "nick.mckay2@gmail.com",
                       updateWebpages = TRUE,
                       updateLipdverse = FALSE,
                       standardizeTerms = FALSE,
                       serialize = TRUE),
 # updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
 data2a = getQcInfo(params,data1),
 data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)

# HoloceneAbruptChange ----------------------------------------------------


HoloceneAbruptChange <- drake_plan(
  params = buildParams("HoloceneAbruptChange",
                       "/Users/nicholas/Dropbox/lipdverse/database",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1u4vWrSsXv_6O16juL4WCgaJdFMxBqwerBi6BGhb3IgQ",
                       lastUpdateId = "1Nb_1lIlsAy6QH6HqGwsNNSvJDjcSqmyjWYDUmccLVt4",
                       googEmail = "nick.mckay2@gmail.com",
                       updateWebpages = TRUE,
                       updateLipdverse = TRUE,
                       qcStandardizationCheck = TRUE,
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  # updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)


# test --------------------------------------------------------------------


test <- drake_plan(
  params = buildParams("test",
                       "/Users/nicholas/Dropbox/lipdverse/testDatabase",
                       "/Users/nicholas/Dropbox/lipdverse/htmlTest/",
                       qcId = "1P0_e-frsQIYFLjLBiJTfouEbdIhMW7UPfYGZBnaDep0",
                       lastUpdateId = "1RbAs0qRWqvHCUfI7q_Er5UKAxRy-otRh7pdM2PKYCHw",
                       googEmail = "nick.mckay2@gmail.com",
                       recreateDataPages = TRUE,
                       updateLipdverse = FALSE,
                       updateWebpages = TRUE,
                       serialize = TRUE,
                       standardizeTerms = FALSE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)


# RapidArcticWarming ------------------------------------------------------


RAW <- drake_plan(
  params = buildParams("RapidArcticWarming",
                       "/Users/nicholas/Dropbox/lipdverse/database",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1tYuhgaDPx1HxdSneL0Nl1Aq7LIM14jzbn5Ke55ha_z0",
                       lastUpdateId = "1OEUGZrqo5Ipz8lBZy9hvxtOeOPOc38sswv3-laFBobU",
                       googEmail = "nick.mckay2@gmail.com",
                       updateWebpages = TRUE,
                       qcStandardizationCheck = FALSE,
                       dontLoadEnsemble = FALSE,
                       ageOrYear = "age",
                       updateDatasetsInCompilationFromInThisCompilation = FALSE,
                       updateLipdverse = FALSE,
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)


# CoralHydro2k ------------------------------------------------------------


CH2k <- drake_plan(
  params = buildParams("CoralHydro2k",
                       "/Users/nicholas/Dropbox/lipdverse/CoralHydro2k/",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1FJAZrPlqc8rYT7cb1sMwEHnpyMSGcQ96MFGsKYJydgU",
                       lastUpdateId = "1N1F1pmepvW3r0l8Wqqm0ibp7MmNJnwAml-quPDQ1wzs",
                       googEmail = "nick.mckay2@gmail.com",
                       updateWebpages = TRUE,
                       qcStandardizationCheck = FALSE,
                       ageOrYear = "year",
                       updateLipdverse = FALSE,
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)


# GBRCD -------------------------------------------------------------------


GBRCD <- drake_plan(
  params = buildParams("GBRCD",
                       "/Users/nicholas/Dropbox/lipdverse/GBRCD/",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1TRdVzs-ZA0betJ3GauiHHiCf3liZsy-S2oBqxh8d7PE",
                       lastUpdateId = "1xIRyBKa9NTfFxkSCuedQPGKwZotHUIIV_PbVey1uLTw",
                       googEmail = "nick.mckay2@gmail.com",
                       qcStandardizationCheck = FALSE,
                       projVersion = "1_0_1",
                       updateWebpages = TRUE,
                       ageOrYear = "year",
                       updateLipdverse = FALSE,
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)


# Temp12k -----------------------------------------------------------------


Temp12k <- drake_plan(
  params = buildParams("Temp12k",
                       "/Users/nicholas/Dropbox/lipdverse/database/",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1JEm791Nhd4fUuyqece51CSlbR2A2I-pf8B0kFgwynug",
                       lastUpdateId = "1qLRMCfDMbTyffJBWlIj3Zw4CAhJY2SECIY-ckcZ2Wak",
                       googEmail = "nick.mckay2@gmail.com",
                       updateWebpages = TRUE,
                       ageOrYear = "age",
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)

# Temp24k -----------------------------------------------------------------


Temp24k <- drake_plan(
  params = buildParams("Temp24k",
                       "/Users/nicholas/Dropbox/lipdverse/database/",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "17XaSH1MNCtBI6ftEnTOHgy9C6mtXvYpR7JCNUNT-vI8",
                       lastUpdateId = "1BHf1RNwjFkDCs57CxMEbbg3zWmk15Od7HL5wi3FbkK8",
                       googEmail = "nick.mckay2@gmail.com",
                       qcStandardizationCheck = FALSE,
                       updateWebpages = FALSE,
                       updateLipdverse = FALSE,
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)


# LakeStatus21k -----------------------------------------------------------
LakeStatus21k <- drake_plan(
  params = buildParams("LakeStatus21k",
                       "/Users/nicholas/Dropbox/lipdverse/database/",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1J-lsbE2f98SaMXQ0u-sOfnCS6_nNBz54kU22zYVqkqg",
                       lastUpdateId = "1ydOXLOG8S82LRhb6m0vXH7JpGpziKn_ZDwH7O_fyb-k",
                       googEmail = "nick.mckay2@gmail.com",
                       qcStandardizationCheck = FALSE,
                       projVersion = "1_0_0",
                       updateWebpages = FALSE,
                       updateLipdverse = FALSE,
                       standardizeTerms = FALSE,
                       serialize = FALSE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)

# iso2k -------------------------------------------------------------------


iso2k <- drake_plan(
  params = buildParams("iso2k",
                       "/Users/nicholas/Dropbox/lipdverse/database/",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1jdSQqUkR_YuJO8Hsp2x3hOhrW19a0SL6wB2mbp4DA2M",
                       lastUpdateId = "1m0-obq3etFf8fvcfN-jrxsYOHqDV7lcsMpZmidxZKis",
                       googEmail = "nick.mckay2@gmail.com",
                       recreateDataPages = FALSE,
                       updateWebpages = TRUE,
                       qcStandardizationCheck = FALSE,
                       ageOrYear = "year",
                       serialize = TRUE,
                       standardizeTerms = FALSE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)

# Pages2kTemperature ------------------------------------------------------


pages2k <- drake_plan(
  params = buildParams("Pages2kTemperature",
                       "/Users/nicholas/Dropbox/lipdverse/database/",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1_ZvQXV-jXMLi7DSC9vc8tAXdlimfkYrna0fKbIB_5Og",
                       lastUpdateId = "11Vh1iCxt0bEq4a8jlM3uawiPX-C8un8i8GUji3S8guA",
                       googEmail = "nick.mckay2@gmail.com",
                       recreateDataPages = FALSE,
                       updateWebpages = TRUE,
                       qcStandardizationCheck = FALSE,
                       ageOrYear = "year",
                       dontLoadEnsemble = FALSE,
                       serialize = TRUE,
                       standardizeTerms = FALSE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)



# LegacyClimate -----------------------------------------------------------


LegacyClimate <- drake_plan(
  params = buildParams("LegacyClimate-LiPD",
                       "/Users/nicholas/Dropbox/lipdverse/database",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1NkH_zydZILIt8YEUO7JYzwiEDpOBEUiFWXgy0otaNLI",
                       lastUpdateId = "1cWM9rOR83fhDEcznmZDuCnYY101_2L9v52IvmthXZzA",
                       googEmail = "nick.mckay2@gmail.com",
                       ageOrYear = "age",
                       updateWebpages = TRUE,
                       updateDatasetsInCompilationFromInThisCompilation = FALSE,
                       qcStandardizationCheck = FALSE,
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)


# Nam2kDendro -----------------------------------------------------------


Nam2kDendro <- drake_plan(
  params = buildParams("Nam2kDendro",
                       "/Users/nicholas/Dropbox/lipdverse/database",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1M_VjlRT7uF4XDiDyVIj5plNXKUkqoAvqfsizwS4hJhs",
                       lastUpdateId = "1QwGqri1olG18m2OYkbYGolQYu78fYzKjbC5SH3k6dfQ",
                       googEmail = "nick.mckay2@gmail.com",
                       ageOrYear = "year",
                       updateWebpages = TRUE,
                       updateDatasetsInCompilationFromInThisCompilation = FALSE,
                       qcStandardizationCheck = FALSE,
                       projVersion = "1_0_0",
                       standardizeTerms = FALSE,
                       serialize = TRUE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)


# Palmod ------------------------------------------------------------------

# Nam2kDendro -----------------------------------------------------------


PalMod <- drake_plan(
  params = buildParams("PalMod",
                       "/Users/nicholas/Dropbox/lipdverse/database",
                       "/Users/nicholas/Dropbox/lipdverse/html/",
                       qcId = "1tTpZ6lS7ML0K241lESJWLWtWF4fK6BWWylmkeYr6Xys",
                       lastUpdateId = "1_LyL5aafRsq4N7MP9XIzfTcGQMVuVMqh5RXf6n50SOA",
                       googEmail = "nick.mckay2@gmail.com",
                       ageOrYear = "age",
                       updateWebpages = TRUE,
                       updateDatasetsInCompilationFromInThisCompilation = FALSE,
                       qcStandardizationCheck = FALSE,
                       projVersion = "2_0_0",
                       standardizeTerms = FALSE,
                       serialize = FALSE),
  updateNeeded = checkIfUpdateNeeded(params),
  data1 = loadInUpdatedData(params),
  data2a = getQcInfo(params,data1),
  data2 = standardizeQCInfo(params,data2a),
  data3 = createQcFromFile(params,data2),
  data4 = mergeQcSheets(params,data3),
  data5 = updateTsFromMergedQc(params,data4),
  data60 = createDataPages(params,data5),
  data61 = createProjectWebpages(params,data60),
  data7 = updateGoogleQc(params,data61),
  data8 = finalize(params,data7),
  changeloggingAndUpdating(params,data8)
)


drake::make(Hydro21k,lock_envir = FALSE)


#update vocab website
#source("/Users/nicholas/GitHub/lipdverse-website/updateWebserver.R")


#rsync -rvauz --delete /Users/nicholas/Dropbox/lipdverse/html/HoloceneAbruptChange/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/HoloceneAbruptChange

#rsync -rvauz --delete /Users/nicholas/Dropbox/lipdverse/html/Hydro21k/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/Hydro21k

#rsync -rvauz --delete /Users/nicholas/Dropbox/lipdverse/html/Temp24k/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/Temp24k

#rsync -rvauz --delete /Users/nicholas/Dropbox/lipdverse/html/HoloceneHydroclimate/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/HoloceneHydroclimate

#rsync -rvauz --delete /Users/nicholas/Dropbox/lipdverse/html/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse

#rsync -rvauz --delete /Users/nicholas/Dropbox/lipdverse/html/RapidArcticWarming/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/RapidArcticWarming

#rsync -rvauz --delete /Users/nicholas/Dropbox/lipdverse/html/GBRCD/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/GBRCD

#rsync -rvauz /Users/nicholas/Dropbox/lipdverse/html/Pages2kTemperature/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/Pages2kTemperature

#rsync -rvauz /Users/nicholas/Dropbox/lipdverse/html/hydroclimate2k/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/hydroclimate2k
#rsync -rvauz /Users/nicholas/Dropbox/lipdverse/html/LakeStatus21k/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/LakeStatus21k
#rsync -rvauz /Users/nicholas/Dropbox/lipdverse/html/FreeSoda/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/FreeSoda

#rsync -rvauz /Users/nicholas/Dropbox/lipdverse/html/SISAL-LiPD/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/SISAL-LiPD
#rsync -rvauz /Users/nicholas/Dropbox/lipdverse/html/LegacyClimate-LiPD/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/LegacyClimate-LiPD
#rsync -rvauz /Users/nicholas/Dropbox/lipdverse/html/CoralHydro2k/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/CoralHydro2k
#rsync -rvauz /Users/nicholas/Dropbox/lipdverse/html/Nam2kDendro/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/Nam2kDendro
#rsync -rvauz /Users/nicholas/Dropbox/lipdverse/html/PalMod/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/PalMod
#rsync -rvauz /Users/nicholas/Dropbox/lipdverse/html/Temp12k/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/Temp12k


#rsync -rvauz /Users/nicholas/Dropbox/lipdverse/html/data/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/data


#### THIS VERSION DELETES non-mirrored files
#rsync -rvauz --delete /Users/nicholas/Dropbox/lipdverse/html/data/ npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/data

