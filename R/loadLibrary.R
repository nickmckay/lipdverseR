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
    message("Cannot find stored serialized version of database, loading from files.")
    D <- lipdR::readLipd(path)
    saveRDS(D,file = rds)
  }
  return(D)
}
