
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
  blank <- unlist(pub) == ""
  blank[is.na(blank)] <- FALSE

  qna <- unlist(pub) == "NA"
  qna[is.na(qna)] <- FALSE

  ina <- is.na(unlist(pub))
  ina[is.na(ina)] <- FALSE

  inu <- purrr::map_lgl(unlist(pub),is.null)
  inu[is.na(inu)] <- FALSE



  return( all(blank | qna | ina | inu  ))
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
    if(any(is.na(empties))){
      print("got NAs, returning original")
      return(L)
    }
    if(any(empties)){#then remove them
      tr <- which(empties)
      L$pub[tr] <- NULL
    }

    if(length(L$pub)==0){
      L$pub <- vector(mode = "list",length = 1) #this needs to be tested!
      L$pub[[1]]$author <- "missing"
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
    oldKey <- matrix(NA,nrow = length(TS))
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
  vars <- pullTsVariable(TS,"paleoData_variableName")
  units <- pullTsVariable(TS,"paleoData_units")

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



#first remove all empty interpretations

#' Interpretation cleaner
#'
#' @param ii
#'
#' @return clean interp
#' @export
interpCleaner <- function(ii){
  if(length(ii)==1){
    if(all(ii == "") | all(is.na(ii))){
      ii <- NULL
    }
  }
  return(ii)
}

#' remove empty interpretations from TS instance
#'
#' @param tsi
#'
#' @return tsi
#' @export
removeEmptyInterpretationsFromTs <- function(tsi){

tsi <- map(tsi,interpCleaner)

torem <- which(map_lgl(tsi,is.null))
if(length(torem)>1){
  tsi <- tsi[-torem]
}

#get the interpretations
ai <- names(tsi)[which(str_detect(names(tsi),"interpretation"))]

#get the scopes
as <- ai[which(str_detect(ai,"scope"))]

for(i in 1:length(as)){
  tn <- str_extract(as[i],"[0-9]")
  if(sum(str_detect(ai,tn))==1){#then it's the only one, remove it
    tsi[[as[i]]] <- NULL
  }
}

torem <- which(map_lgl(tsi,is.null))
if(length(torem)>1){
  tsi <- tsi[-torem]
}


#get the interpretations
ai <- names(tsi)[which(str_detect(names(tsi),"interpretation"))]

tn <- as.numeric(str_extract(ai,"[0-9]"))

utn <- sort(unique(tn))
if(length(utn) != max(utn)){#then renumber
  newname <- ai
  for(i in 1:length(utn)){
    ind <- which(tn==utn[i])
    newname[ind] <- str_replace(string = ai[ind], "[0-9]",as.character(i))
  }
  names(tsi)[which(str_detect(names(tsi),"interpretation"))] <- newname
}



return(tsi)
}


#' Fix issues from files imported from excel for climate 12k
#'
#' @param L
#'
#' @return L
#' @export
fixExcelIssues <- function(L){
  ts <- extractTs(L)

  vn <- pullTsVariable(ts,"paleoData_variableName")
  vno <- try(pullTsVariable(ts,"paleoData_variableNameOriginal"))
  if(class(vno)=="try-error"){
    vno <- vn
  }

  #first, correct age units
  wa <- which(vn == "age")
  if(length(wa)>0){
    for(i in 1:length(wa)){
      if(length(ts[[wa[i]]]$paleoData_units)==0){
        ts[[wa[i]]]$paleoData_units <- "yr BP"
      }
      if(ts[[wa[i]]]$paleoData_units == "degC"){
        ts[[wa[i]]]$paleoData_units <- "yr BP"
      }
    }
  }

  #second, deal with reliability issues
  rel <- which(vn == "N")
  temp1 <- which(vno == "TemperatureReconstruction1")

  temp2 <- which(vno == "TemperatureReconstruction2")

  temp3 <- which(vno == "TemperatureReconstruction3")

  if(length(rel)>0 & length(temp1)>0){
    for(j in 1:length(temp1)){
      ts <- unreliable(ts,temp1[j],rel[1])
      ts[[rel[1]]]$paleoData_variableName <- "reliable"
    }
  }
  if(length(rel)>1 & length(temp2)>0){
    for(j in 1:length(temp2)){
      ts <- unreliable(ts,temp2[j],rel[2])
      ts[[rel[2]]]$paleoData_variableName <- "reliable"

    }
  }
  if(length(rel)>2 & length(temp3)>0){
    for(j in 1:length(temp3)){
      ts <- unreliable(ts,temp3[j],rel[3])
      ts[[rel[3]]]$paleoData_variableName <- "reliable"

    }
  }

  newL <- collapseTs(ts)
  return(newL)

}



#' Deal with unreliable samples
#'
#' @param ts
#' @param ti
#' @param ri
#'
#' @return ts
#' @export
unreliable <- function(ts,ti,ri){
  rel <- ts[[ri]]$paleoData_values
  bad <- which(stringr::str_detect(tolower(rel),"n"))
  if(is.null(ts[[ti]]$paleoData_QCnotes)){
    alreadyFixed <- FALSE
  }else{
    alreadyFixed <- str_detect("; removed unreliable values",string = ts[[ti]]$paleoData_QCnotes)
  }

  if(length(bad) > 0){#then fix it
    if(alreadyFixed){
      print(str_c(ts[[1]]$dataSetName,"-",ts[[ti]]$paleoData_variableNameOriginal,": already fixed!"))
    }else{
      print(str_c(ts[[1]]$dataSetName,"-",ts[[ti]]$paleoData_variableNameOriginal,": moving and removing unreliable values"))
      temp <- ts[[ti]]$paleoData_values

      #bad values
      nas <- matrix(NA,nrow = length(rel))
      nas[bad] <- temp


      tempBad <- ts[[ti]] #copy the temperature values
      tempBad$paleoData_variableName <- str_c("nonReliable",ts[[ti]]$paleoData_variableName)
      tempBad$paleoData_variableNameOriginal <- NA

      tempBad$paleoData_TSid <- createTSid()
      tempBad$paleoData_useInGlobalTemperatureAnalysis <- FALSE
      tempBad$paleoData_values <- nas

      ts[[length(ts)]] <- tempBad
      ts[[ti]]$paleoData_values[bad] <- NA
      ts[[ti]]$paleoData_QCnotes <- str_c(ts[[ti]]$paleoData_QCnotes,"; removed unreliable values")
    }
  }
  return(ts)
}

#resolve daff conflicts
#' Fixes conflicts that shouldn't exist
#'
#' @param qc
#'
#' @return qc
#' @export
resolveDumbConflicts <- function(qc){
  wc <- daff::which_conflicts(qc)
  if(length(wc)>0){
    for(i in 1:length(wc)){
      tr <- qc[wc[i],]
      wcell <- which(stringr::str_detect(tr,fixed("(((")))

      for(j in 1:length(wcell)){
        tv <- qc[wc[i],wcell[j]]

        last <- stringr::str_trim(stringr::str_extract(tv,"[^)]+$"))
        conf <- stringr::str_split(last," /// ",simplify = T)

        if(conf[1] == conf[2]){
          qc[wc[i],wcell[j]] <- conf[1]
        }
      }
    }
  }
  return(qc)
}

