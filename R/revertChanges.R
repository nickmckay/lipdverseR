#recover elevation changes

recoverElevationChange <- function(L){

  if(is.na(L$geo$elevation)){
    elevChangePatternPos <- "geo_elevation: '\\d{1,}' has been replaced by ''"
    elevChangePatternNeg <- "geo_elevation: '-\\d{1,}' has been replaced by ''"

    #see if elevation was ever removed.
    er <- map(L$changelog,
                  \(x) which(
                    stringr::str_detect(
                      x$changes$`Geographic metadata`,
                      pattern = c(elevChangePatternPos,elevChangePatternNeg))
                    ))

    #find the most recent change
    which.cl <- min(which(map_dbl(er,length) > 0))

    if(is.finite(which.cl)){

    tcp <- which(stringr::str_detect(L$changelog[[which.cl]]$changes$`Geographic metadata`,pattern = elevChangePatternPos))
    tcn <- which(stringr::str_detect(L$changelog[[which.cl]]$changes$`Geographic metadata`,pattern = elevChangePatternNeg))

    tc <- min(tcp,tcn)

    if(length(tc) == 1){
      nep <- as.numeric(stringr::str_extract(L$changelog[[which.cl]]$changes$`Geographic metadata`[tc],"\\d{1,}"))
      nen <- as.numeric(stringr::str_extract(L$changelog[[which.cl]]$changes$`Geographic metadata`[tc],"-\\d{1,}"))

      if(!is.na(nen)){
        ne <- nen
      }else if(!is.na(nep)){
        ne <- nep
      }

      if(!is.null(ne)){
        print(glue::glue("Recovered elvation: {ne} from {L$dataSetName}"))
        L$geo$elevation <- ne
      }
    }
    }else{
      print(glue::glue("No previous elevation found in changelog"))
      return(L)
    }
  }

  return(L)
}


#D2 <- purrr::map(D,recoverElevationChange)


#only get those with no elevations
# noElev <- which(purrr::map_lgl(D,\(x) is.na(x$geo$elevation)))
#
# Df <- purrr::map(D[noElev],recoverElevationChange)
#
# noElevNow <- which(purrr::map_lgl(Df,\(x) is.na(x$geo$elevation)))
#
# D2update <- Df[-noElevNow]
#
# walk(D2update,addLipdToDatabase)

