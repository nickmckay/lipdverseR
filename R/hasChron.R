hasChron <- function(L){
  if(any(names(L)=="chronData")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


