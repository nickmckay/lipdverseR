hasDuplicateTsids <- function(L){
  ts <- extractTs(L) %>% ts2tibble()

  if(any(duplicated(ts$paleoData_TSid))){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
