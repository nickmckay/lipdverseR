#! /usr/bin/env Rscript
#PATH=/usr/local/bin:/usr/local/sbin:~/bin:/usr/bin:/bin:/usr/sbin:/sbin


.libPaths("/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/MacOS/pandoc")
library(lipdverseR)
library(googledrive)

(updateProject("globalHolocene","~/Dropbox/HoloceneLiPDLibrary/masterDatabase/","~/GitHub/lipdverse/html/",qcId = "1JEm791Nhd4fUuyqece51CSlbR2A2I-pf8B0kFgwynug",lastUpdateId = "1qLRMCfDMbTyffJBWlIj3Zw4CAhJY2SECIY-ckcZ2Wak",googEmail = "nick.mckay2@gmail.com",updateWebpages = TRUE))

#drive_share(as_id("1JEm791Nhd4fUuyqece51CSlbR2A2I-pf8B0kFgwynug"),role = "writer", type = "anyone")

system("~/Dropbox/Scripts/update_lipdverse")
