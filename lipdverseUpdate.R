#! /usr/bin/env Rscript
#PATH=/usr/local/bin:/usr/local/sbin:~/bin:/usr/bin:/bin:/usr/sbin:/sbin


.libPaths("/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/MacOS/pandoc")
library(lipdverseR)
setwd("~/GitHub/lipdverseR/")

updateProject("globalHolocene","~/Dropbox/HoloceneLiPDLibrary/masterDatabase/","~/GitHub/lipdverse/html/",qcId = "1JEm791Nhd4fUuyqece51CSlbR2A2I-pf8B0kFgwynug",lastUpdateId = "1qLRMCfDMbTyffJBWlIj3Zw4CAhJY2SECIY-ckcZ2Wak",googEmail = "nick.mckay2@gmail.com",updateWebpages = TRUE,standardizeTerms = TRUE)

# updateProject("PalMod","~/Dropbox/LiPD/PalMod/","~/GitHub/lipdverse/html/",qcId = "18Ur3k8Dfub9y3arfO_RKZYFUcYSfYAGy_vuKznv9MLM",googEmail = "nick.mckay2@gmail.com",updateWebpages = TRUE,lastUpdateId = "1WujcuW4rPI8G9iyxeVlNB8oCCsj8K7ggqeBMkkE8zXA",standardizeTerms = FALSE)

#updateProject("iso2k","~/Dropbox/LiPD/iso2k/masterDatabase/","~/GitHub/lipdverse/html/",qcId = "1jdSQqUkR_YuJO8Hsp2x3hOhrW19a0SL6wB2mbp4DA2M",googEmail = "nick.mckay2@gmail.com",updateWebpages = TRUE,lastUpdateId = "1m0-obq3etFf8fvcfN-jrxsYOHqDV7lcsMpZmidxZKis",standardizeTerms = TRUE,ageOrYear = "year")

#drive_share(as_id("1jdSQqUkR_YuJO8Hsp2x3hOhrW19a0SL6wB2mbp4DA2M"),role = "writer", type = "anyone")


#drive_share(as_id("1JEm791Nhd4fUuyqece51CSlbR2A2I-pf8B0kFgwynug"),role = "writer", type = "anyone")
#createProjectDashboards(D,TS,"~/GitHub/lipdverse/html/","iso2k","0_14_2",currentVersion = TRUE)
system("~/Dropbox/Scripts/update_lipdverse")
