#recover elevation changes

recoverElevationChange <- function(L){

  if(is.na(L$geo$elevation)){
    elevChangePattern <- "geo_elevation: '\\d{1,}' has been replaced by ''"
    tc <- which(stringr::str_detect(L$changelog[[1]]$changes$`Geographic metadata`,pattern = elevChangePattern) )
    if(length(tc) == 1){
      ne <- as.numeric(stringr::str_extract(L$changelog[[1]]$changes$`Geographic metadata`[tc],"\\d{1,}"))
      if(!is.null(ne)){
        print(glue::glue("Recovered elvation: {ne} from {L$dataSetName}"))
        L$geo$elevation <- ne
      }
    }
  }

  return(L)
}


#D2 <- purrr::map(D,recoverElevationChange)
