
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


#' Test empty pubs
#'
#' @param pub
#'
#' @return boolean
#' @export
isEmptyPub <- function(pub){
  return(all(unlist(pub) == "" || unlist(pub) == "NA" | is.na(unlist(pub)) | is.null(unlist(pub))))
}

#' remove empty pubs
#'
#' @param L
#'
#' @return L
#' @export
removeEmptyPubs <- function(L){
  if("pub" %in% names(L)){#if there is a pub section
    empties <- purrr::map_lgl(L$pub,isEmptyPub)
    if(any(empties)){#then remove them
      tr <- which(empties)
      L$pub[tr] <- NULL
    }
  }
  return(L)
}


#' Get values for replacement from a converter
#'
#' @param key
#' @param conv
#' @import purrr
#' @import stringr
#' @return
#' @export

getVals <- function(key,conv){
  alts <- conv
  logTab <- map_df(alts,function(x) {tolower(key) == fixed(tolower(x))})
  repi <- which(rowSums(logTab,na.rm = T) > 0)
  if(length(repi) > 1){
    warning("Multiple matches for conversion - returning original")
    out <- key
  }else if(length(repi) == 0){
    out <- key
  }else{
    out <- conv$`Official Name`[repi]
  }
  return(out)
}


#' standardize values in a TS
#'
#' @param TS
#' @param tsKey
#' @param googId
#' @import purrr
#' @import geoChronR
#'
#' @return TS
#' @export
standardizeValues <- function(TS,tsKey,googId){

  conv <- getConverter(googId,howLong = 30)
  #check for duplicates? not yet

  key <- geoChronR::pullTsVariable(TS,tsKey)
  newKeys <- map_chr(key,getVals,conv)

  #find differences
  diffKeys <- which(newKeys != key)

  #does an original column already exist?
  allNames <- unique(unlist(sapply(TS,names)))
  oName <- paste0(tsKey,"Original")
  if(oName %in% allNames){#it already exists
    oldKey <- geoChronR::pullTsVariable(TS,oName)
    createNew = FALSE
  }else{
    oldKey <- matrix("",nrow = length(TS))
    createNew = TRUE
  }
  oldKey[diffKeys] <- key[diffKeys]

  #push variables back
    TS <- geoChronR::pushTsVariable(TS,tsKey,newKeys)
    TS <- geoChronR::pushTsVariable(TS,oName,oldKey,createNew = createNew)
  return(TS)

}

#' Standardize keys in a TS
#'
#' @param TS
#' @param renamingDirectoryId
#' @description renames keys based on a directory of google spreadsheets
#' @return
#' @export
standardizeTsValues <- function(TS, renamingDirectoryId  = "1b-4arcNxxGsArCM6XfjW6y6yAp85BMlF-Gn8au4Zygg"){
  #get the directory
  direc <- getConverter(renamingDirectoryId,howLong = 1)

  #get all the names in the TS
  allNames <- unique(unlist(sapply(TS,names)))

  #allNames after underscore
  allNamesAU <- stringr::str_extract(allNames,"(?<=_)[a-zA-Z0-9]+")

  allNamesAU[is.na(allNamesAU)] <- allNames[is.na(allNamesAU)]

  for(i in 1:length(direc$key)){
    key <- direc$key[i]
    if(str_detect(key,"_")){#if there's an underscore, use the whole term
      names2get <- allNames[which(allNames==key)]
    }else{#otherwise only look after underscore
      names2get <- allNames[which(allNamesAU==key)]
    }
    if(length(names2get)==0){
      print(stringr::str_c("no matches found for key: ", key))
      next
    }
    for(name in names2get){
    print(stringr::str_c("standardizing ",name,"..."))
      TS <- standardizeValues(TS,name,direc$googID[i])
    }
  }
  return(TS)
}



#' Get a converter from google drive
#'
#' @param googId
#' @import lubridate
#' @import googledrive
#' @import readr
#' @return
#' @export
getConverter <- function(googId,howLong = 30){

  fname <- file.path(tempdir(),stringr::str_c(googId,".csv"))

  #check time
  if(is.na(lubridate::now()-file.mtime(fname)) | lubridate::now()-file.mtime(fname) > howLong){

    #download name conversion
    convo <- googledrive::as_id(googId) %>%
      googledrive::drive_get() %>%
      googledrive::drive_download(path = fname,overwrite = T) %>%
      dplyr::select(local_path) %>%
      as.character() %>%
      readr::read_csv()

  }else{

    convo <- readr::read_csv(fname)

  }

  return(convo)
}


#' Convert kiloyears in a TS object
#'
#' @param TS
#'
#' @return TS
#' @export
fixKiloyearsTs <- function(TS){
  vars <- pullTsVariable(TS,"paleodata_variableName")
  units <- pullTsVariable(TS,"paleodata_units")

  isAge <- which(vars=="age")
  if(length(isAge)>1){
    for(i in isAge){
      if(!is.na(units[i])){
        if(stringr::str_detect(tolower(units[i]),"ky") | stringr::str_detect(tolower(units[i]),"ka")){
          TS[[i]]$paleoData_values <- TS[[i]]$paleoData_values*1000
          TS[[i]]$paleoData_units <- "yr bp"
          #print(paste("multiplied age column with units",units[i],"by 1000"))
        }
      }
    }
  }
  return(TS)

}

