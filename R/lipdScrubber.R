
#' remove all instances of a variable from a TS
#'
#' @param TS
#' @param variable
#'
#' @return
#' @export
removeVariable <- function(TS,variable){
  for(i in 1:length(TS)){
   TS[[i]][[variable]] <- NULL
  }
  return(TS)
}





#' Fix pubYear problem
#'
#' @param TS
#'
#' @return TS
#' @export
fix_pubYear <- function(TS){
#remove pubYear, merge into year
  nts <- TS
  allNames <- unique(unlist(sapply(TS,names)))
  pyi <- grepl("pubYear",allNames)
  if(sum(pyi)==0){
    print("no instances of pubYear! Returning input TS")
    return(TS)
  }

  py <- allNames[pyi]
  y <- stringr::str_replace(py,"_pubYear","_year")

  for(i in 1:length(py)){
    pubYear <- geoChronR::pullTsVariable(TS,py[i])
    year <- try(geoChronR::pullTsVariable(TS,y[i]))
    if(class(year) == "try-error"){
      year <- matrix(data = NA,nrow = length(pubYear),ncol = 1)
    }
    ey <- which(is.na(year) | year == "NA" | year == "0")#eligible for replacement
    year[ey] <- pubYear[ey]

    # replace bad year names
    by <- which(year == "NA" | year == "0")#bad
    year[by] <- NA

    #force to numeric
    year <- as.numeric(year)

    #push back to TS
    nts <- geoChronR::pushTsVariable(TS = nts,variable = y[i],vec = year,createNew = TRUE)

    nts <- removeVariable(nts,variable = py[i])

    #announce this
    print(stringr::str_c("merged ",py[i]," into ",y[i]," and did a bit of scrubbing too; then deleted ",py[i]))
  }

  return(nts)
}

#remove empty pubs
isEmptyPub <- function(pub){
  return(all(unlist(pub) == "" || unlist(pub) == "NA" | is.na(unlist(pub)) | is.null(unlist(pub))))
}

removeEmptyPubs <- function(L){
  if("pub" %in% names(L)){#if there is a pub section
    empties <- purrr::map_lgl(L$pub,isEmptyPub)
    if(any(empties)){#then remove them
      tr <- which(empties)
      L$pub[[tr]] <- NULL
    }
  }
  return(L)
}




