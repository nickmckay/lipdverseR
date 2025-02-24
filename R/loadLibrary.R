loadLipdverseDatabase <- function(path = "~/Dropbox/lipdverse/database/"){
  #get all lipdfiles in database
  allLipd <- list.files(path,pattern = ".lpd",full.names = TRUE)

  #check md5sum
  allmd5 <- tools::md5sum(allLipd)
  md5f <- file.path(tempdir(),"lipdverseAllMd5.txt")

  write(allmd5,file = md5f)

  tc <- tools::md5sum(md5f)
  rds <- file.path(path,paste0(tc,".RDS"))

  if(file.exists(rds)){
    message("Loading stored serialized version of database")
    D <- readRDS(rds)
  }else{
    message("Cannot find stored serialized version of database, removing old RDS files, and loading from files.")
    toDelete <- list.files(path,pattern = ".RDS",full.names = TRUE)
    unlink(x = toDelete)
    future::plan(future::multisession,workers = 16)
    D <- lipdR::readLipd(path,parallel = TRUE)
    saveRDS(D,file = rds)
  }
  return(D)
}
