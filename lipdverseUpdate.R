#! /usr/bin/env Rscript
#PATH=/usr/local/bin:/usr/local/sbin:/Users/npm4/bin:/usr/bin:/bin:/usr/sbin:/sbin


.libPaths("/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/MacOS/pandoc")
library(lipdverseR)
library(magrittr)
library(tidyverse)
library(lipdR)

setwd("/Users/nicholas/GitHub/lipdverseR/")

#updateProject("Temp12k","/Users/npm4/Dropbox/lipdverse/database","/Users/npm4/GitHub/lipdverse/html/",qcId = "1JEm791Nhd4fUuyqece51CSlbR2A2I-pf8B0kFgwynug",lastUpdateId = "1qLRMCfDMbTyffJBWlIj3Zw4CAhJY2SECIY-ckcZ2Wak",googEmail = "nick.mckay2@gmail.com",updateWebpages = TRUE,standardizeTerms = TRUE)

#updateProject("globalHolocene","/Users/npm4/Dropbox/lipdverse/database","/Users/npm4/GitHub/lipdverse/html/",qcId = "needANewOne",lastUpdateId = "needANewOne",googEmail = "nick.mckay2@gmail.com",updateWebpages = FALSE,standardizeTerms = FALSE)

updateProject("wNAm","/Users/npm4/Dropbox/lipdverse/database","/Users/npm4/GitHub/lipdverse/html/",qcId = "166sUZ3rnjizRv2KCtcaaEj8NKIQgW6L1_z78VtNgOM4",lastUpdateId = "180Cig1Z2qFUKq-kVe0teQYkNrZps8yPZDgWu8xExciU",googEmail = "nick.mckay2@gmail.com",updateWebpages = TRUE,standardizeTerms = TRUE)

system("rsync -rv --delete ~/GitHub/lipdverse/html/wNAm/ npm4@linux.cefns.nau.edu:/www/sites/cefns/seses/lipdverse/wNAm/")


#updateProject("HoloceneAbruptChange","/Users/npm4/Dropbox/lipdverse/database","/Users/npm4/GitHub/lipdverse/html/",qcId = "1u4vWrSsXv_6O16juL4WCgaJdFMxBqwerBi6BGhb3IgQ",lastUpdateId = "19yPTdIuwr-IN2YOAhqIsZliNF91OyGiKFTCKZw4MZXE",googEmail = "nick.mckay2@gmail.com",updateWebpages = TRUE,standardizeTerms = TRUE)


updateProject("test","/Users/nicholas/Dropbox/lipdverse/testDatabase","/Users/nicholas/Dropbox/lipdverse/html/",qcId = "1P0_e-frsQIYFLjLBiJTfouEbdIhMW7UPfYGZBnaDep0",lastUpdateId = "1RbAs0qRWqvHCUfI7q_Er5UKAxRy-otRh7pdM2PKYCHw",googEmail = "nick.mckay2@gmail.com",updateWebpages = TRUE,standardizeTerms = FALSE)

# updateProject("PalMod","/Users/npm4/Dropbox/LiPD/PalMod/","/Users/npm4/GitHub/lipdverse/html/",qcId = "18Ur3k8Dfub9y3arfO_RKZYFUcYSfYAGy_vuKznv9MLM",googEmail = "nick.mckay2@gmail.com",updateWebpages = TRUE,lastUpdateId = "1WujcuW4rPI8G9iyxeVlNB8oCCsj8K7ggqeBMkkE8zXA",standardizeTerms = FALSE)

#updateProject("iso2k","/Users/npm4/Dropbox/LiPD/iso2k/masterDatabase/","/Users/npm4/GitHub/lipdverse/html/",qcId = "1jdSQqUkR_YuJO8Hsp2x3hOhrW19a0SL6wB2mbp4DA2M",googEmail = "nick.mckay2@gmail.com",updateWebpages = TRUE,lastUpdateId = "1m0-obq3etFf8fvcfN-jrxsYOHqDV7lcsMpZmidxZKis",standardizeTerms = TRUE,ageOrYear = "year")

#drive_share(as_id("1jdSQqUkR_YuJO8Hsp2x3hOhrW19a0SL6wB2mbp4DA2M"),role = "writer", type = "anyone")


#drive_share(as_id("1JEm791Nhd4fUuyqece51CSlbR2A2I-pf8B0kFgwynug"),role = "writer", type = "anyone")
#createProjectDashboards(D,TS,"/Users/npm4/GitHub/lipdverse/html/","iso2k","0_14_2",currentVersion = TRUE)
#system("/Users/npm4/Dropbox/Scripts/update_lipdverse")
