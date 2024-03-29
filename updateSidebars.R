allf <- list.dirs("~/Dropbox/lipdverse/html/data")

hu <- which(str_detect(allf,"_"))

allfu <- allf[hu]

library(glue)
library(lipdR)
for(i in 1:length(allfu)){
  log <- capture.output({
    L <- readLipd(file.path(allfu[i],"lipd.lpd"))
  })
  try(createSidebarHtml(L))
  if(i%%100 == 0){
    cat(i/length(allfu) * 100,"% \r")
  }
}


