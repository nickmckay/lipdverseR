hasChron <- function(L){
  if(any(names(L)=="chronData")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


hasChronEnsemble <- function(L){
  hce <- FALSE

  if(any(names(L)=="chronData")){
    for(i in 1:length(L$chronData)){
      if(any(names(L$chronData[[i]])=="model")){
        for(j in 1:length(L$chronData[[i]]$model)){
          if(any(names(L$chronData[[i]]$model[[j]])=="ensembleTable")){
            hce <- TRUE
            return(hce)
          }
        }
      }
    }
  }
  return(hce)
}
