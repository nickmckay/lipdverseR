library(lipdR)
library(purrr)

alllipd <- list.files("/Volumes/data/Dropbox/newRecordsForRAW/PendingAddtoLipd/",pattern = "*.lpd",recursive = TRUE,full.names = TRUE)


N <- map(alllipd,readLipd)

map(N,addLipdToDatabase)

bad <- which(!map_lgl(ts$age,~ all(is.numeric(.x))))
ts$dataSetName[bad[58]]
