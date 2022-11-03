nf <- list.files("~/Dropbox/newRecordsForRAW/4.PendingAddtoLipd/",recursive = TRUE,pattern = "*.lpd",full.names = TRUE)
library(lipdR)
library(purrr)
N <- readLipd(nf)

map(N,lipdR:::validLipd)

walk(N,addLipdToDatabase)
