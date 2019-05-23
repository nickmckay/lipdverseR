#' Flatten authors list to a string
#'
#' @param vec
#'
#' @return
#' @export
flattenAuthors <- function(vec){

if(class(vec)=="matrix"){#handle specially
  mat <- apply(vec,2,function(x){sapply(x,"[[","name")})
  nvec <- apply(mat,2,paste,collapse = " ; ")
}else if(class(vec)=="list"){
  nvec <- matrix(NA,nrow = length(vec))
  for(i in 1:length(vec)){
    tl <- vec[[i]]
    if(is.character(tl)){
      nvec[i] <- tl
    }else if(is.list(tl)){
      nvec[i] <- paste(sapply(tl,"[[","name"),collapse = " ; ")
    }

  }
}
  return(nvec)
}


#' get google qc sheet
#'
#' @param qcSheetId Google sheet key
#' @description returns a qc sheet. Also saves a csv file called "googleQC.csv"
#' @return qcs qc data frame
#' @import googledrive
#' @import here
#' @import readr
#' @export
#'
#' @examples
getGoogleQCSheet <- function(qcSheetId){
  #download qc sheet
  setwd(here::here())
  x <- googledrive::drive_get(googledrive::as_id(qcSheetId))
  qc <- googledrive::drive_download(x,path = here::here("googleQC.csv"),type = "csv",overwrite = T)

  #remove any special characters
  rosetta <- lipdverseR::rosettaStone()
  qcs <- readr::read_csv(here::here("googleQC.csv")) %>%
  purrr::map_df(lipdverseR::replaceSpecialCharacters,rosetta)

  return(qcs)
}


#TO DO - update to export error logging.
#' update a split Timeseries object from a google QC sheet
#' @export
#' @param sTS a split timeseries object
#' @param qcs a QC data.frame
#' @import googledrive
#' @importFrom magrittr %>%
#' @import readr
#' @import stringr
#' @import here
#' @import lipdR
#' @import dplyr
#' @import geoChronR
#' @return an updated sTS
#'
updateFromQC <- function(sTS,qcs){

  #setup reporting
  report <- c()
  reportY <- c()
  noMatch <- c()



  #download name conversion
  convo <- googledrive::as_id("1T5RrAtrk3RiWIUSyO0XTAa756k6ljiYjYpvP67Ngl_w") %>%
    googledrive::drive_get() %>%
    googledrive::drive_download(path = here::here("convo.csv"),overwrite = T) %>%
    dplyr::select(local_path) %>%
    as.character() %>%
    readr::read_csv()

  #convert names
  qcNames <- names(qcs)
  tsNames <- c()
  for(i in 1:length(qcNames)){
    ind <- which(convo$qcSheetName %in% qcNames[i])
    if(length(ind)==0){
      print(qcNames[i])
    }else if(length(ind)==1){
      tsNames[i] <- convo$tsName[ind]
    }else{
      stop("multiple convo matches")
    }
  }

  #see if any qcsheet TSids are missing from the database
  TSid <- geoChronR::pullTsVariable(sTS,"paleoData_TSid")
  dsn <- geoChronR::pullTsVariable(sTS,"dataSetName")
  missingTSid <- c()
  missingTSidDsn <- c()

  for(i in 2:nrow(qcs)){
    if(!any(qcs$TSid[i] == TSid,na.rm = T)){
      missingTSid <- c(missingTSid,qcs$TSid[i])
      missingTSidDsn <- c(missingTSidDsn,qcs$dataSetName[i])
    }
  }

  missDf <- cbind(missingTSid,missingTSidDsn)

  #loop through TS and test
  newTS <- sTS

  allNames <- unique(unlist(sapply(sTS,names)))
  missingNames <- allNames[!allNames %in% convo$tsName]
  missingNamesTS <- missingNames[order(missingNames)]

  allNamesConvo <- convo$tsName
  missingNamesConvo <- convo$tsName[!convo$tsName %in% allNames]

  bnames <- c("geo","pub","funding","dataPub")
  dsn <- sapply(sTS,"[[","dataSetName")

  for(i in 1:length(sTS)){
    thisTSid <- sTS[[i]]$paleoData_TSid
    #find which QC row
    qci <- which(qcs$TSid %in% thisTSid)
    #if doesn't exist, report out:
    if(length(qci)==0){
      if(any(grepl(sTS[[i]]$paleoData_variableName,c("year","depth","age")))){
        report = rbind(report,stringr::str_c("TSid: ",thisTSid ," for variableName: ",sTS[[i]]$paleoData_variableName ," in dataset:",sTS[[i]]$dataSetName, " doesn't exist in QC sheet") )
      }else{
        reportY = rbind(reportY,stringr::str_c("TSid: ",thisTSid ," for variableName: ",sTS[[i]]$paleoData_variableName ," in dataset:",sTS[[i]]$dataSetName, " doesn't exist in QC sheet") )
      }
    }else if(length(qci)>1){#then too many matches
      #stop("too many TSid matches")
      report = rbind(report,stringr::str_c("Too many matches for TSid: ",thisTSid ," for variableName: ",sTS[[i]]$paleoData_variableName ," in dataset:",sTS[[i]]$dataSetName) )
    }else{#loop through variables and force an update
      thisTSnames <- unique(c(names(sTS[[i]]),allNamesConvo)) #find names for this ts, combine with convo names for updates
      for(j in 1:length(thisTSnames)){
        rn <- which(tsNames %in% thisTSnames[j])
        # #ignore TS names without matches

        #find matches
        if(length(rn) == 1){
          #check type
          varType <- convo$type[which(convo$tsName %in% thisTSnames[j])]
          if(varType == "character"){
            varFun <- as.character
          }else if(varType == "numeric"){
            varFun <- as.numeric
          }else if(varType == "boolean"){
            varFun <- as.logical
          }else if(varType == "author"){
            varFun <- function(x){
              aut <- stringr::str_split(x," ; ",simplify = TRUE)
              out <- purrr::map(seq_along(aut),function(y){list(name = aut[y])})
              return(list(out))
            }
          }else{
            stop("variable type not recognized")
          }


          #apply to all timeseries from this dataset?
          sname <- stringr::str_split(thisTSnames[j],"_")
          #start with false
          apply2all <- FALSE
          if(length(sname[[1]])==1){#then its base
            apply2all <- TRUE
          }else{
            base <- stringr::str_remove_all(sname[[1]][1],"[0-9]")
            apply2all <- any(grepl(base,bnames))
          }

          #fill it in.
          if(apply2all){#then fill it in for all in dataset
            #print(sname)
            dsni <- which(sTS[[i]]$dataSetName == dsn)
            for(k in 1:length(dsni)){
              if (is.null(varFun(qcs[qci,rn]))){
              newTS[[dsni[k]]][thisTSnames[j]] <- NULL
              }else{
                if(varType == "author"){
                  1+1
                }
              newTS[[dsni[k]]][thisTSnames[j]] <- varFun(qcs[qci,rn])
              }
            }
          }else{#then just for this one timeseries
            newTS[[i]][thisTSnames[j]] <- varFun(qcs[qci,rn])
          }

        }
        if(length(rn) > 1){
          stop("there shouldn't be multiple matches in ts names")
        }
      }
    }#end loop through variables and force an update
  }
  write_csv(x = as.data.frame(report),path = "~/GitHub/lipdverse/updateQc_log.csv")
  return(newTS)
}


#' Create QC data.frame
#' @export
#' @param sTS
#' @import googledrive
#' @importFrom magrittr %>%
#' @import readr
#' @import stringr
#' @import here
#' @import lipdR
#' @import dplyr
#' @import geoChronR
#' @return a data.frame QC sheet
createQCdataFrame <- function(sTS,templateId,to.omit = c("depth","age","year"),to.omit.specific = c("yr")){
  #setup reporting
  report <- c()
  noMatch <- c()
  #
  #download qc sheet template
  setwd(here::here())
  x <- drive_get(as_id(templateId))
  qc <- drive_download(x,path = here::here("template.csv"),type = "csv",overwrite = T)
  qcs <- read_csv(here::here("template.csv"))

  #download name conversion
  convo <- as_id("1T5RrAtrk3RiWIUSyO0XTAa756k6ljiYjYpvP67Ngl_w") %>%
    drive_get() %>%
    drive_download(path = here::here("convo.csv"),overwrite = T) %>%
    select(local_path) %>%
    as.character() %>%
    read_csv()

  #filter rows
  varNames <- pullTsVariable(sTS,"paleoData_variableName")
  uvn <- unique(varNames)


  #identify variables to omit
  toi <- c()
  #pattern search first
  for(to in to.omit){
    toi <- append(toi,which(grepl(to,uvn,ignore.case = T)))
  }
  toi <- unique(toi)

  #specific search second
  for(to in to.omit.specific){
    toi <- append(toi,which(to == uvn))
  }

  #grab them all and filter
  rem.var <- uvn[toi]
  good.in <- which(!varNames %in% rem.var)
  fsTS <- sTS[good.in]

  #Add more filtering options later?


  outRows <- length(fsTS)

  #get all ages and years
  if(any(varNames=="year")){
    allYear <- pullTsVariable(fsTS,"year")
  }else{
    allYear <- vector(mode = "list",length = length(fsTS))
  }

  if(any(varNames=="age")){
    allAge <- pullTsVariable(fsTS,"age")
  }else{
    allAge <- vector(mode = "list",length = length(fsTS))
  }

  #convert years, calculate min/max years
  toRep <- which(sapply(allAge,length)==0 & sapply(allYear,length)>0)

  for(t in toRep){
    allAge[[t]] <- convertAD2BP(allYear[[t]])
  }




  minAge <- sapply(allAge,min,na.rm=TRUE)
  maxAge <- sapply(allAge,max,na.rm=TRUE)

  fsTS <- pushTsVariable(fsTS,"minYear",minAge,createNew = TRUE)
  fsTS <- pushTsVariable(fsTS,"maxYear",maxAge, createNew = TRUE)

  #vectors to create
  toPull <- names(qcs) #get all names from template

  #get all names from TS
  allNames <- unique(unlist(sapply(fsTS,names)))

  #setup qc tibble
  out <- as.data.frame(matrix(NA,nrow = outRows,ncol = length(toPull)))
  names(out) <- toPull
  #out[1,] <- qcs[1,]
  for(i in 1:length(toPull)){
      n2p <- convo$tsName[toPull[i]==convo$qcSheetName]
      if(any(n2p==allNames)){
        vec <- pullTsVariable(fsTS,n2p)
        #check to see if vec is authors

        if(grepl("author",n2p)){
         vec <- flattenAuthors(vec)
        }
      }else{
        print(str_c("Does not exist in TS. Putting an empty column for ",toPull[i]))
        vec <- rep(NA,outRows)
      }

      out[,i] <- vec

  }


  out[is.na(out)] <- ""
  out[out=="NA"] <- ""

  rosetta <- lipdverseR::rosettaStone()
  out <- purrr::map_df(out,lipdverseR::replaceSpecialCharacters,rosetta)



  return(out)
}


#' Create a new QC spreadsheet on google drive
#' @export
#' @param qcdf QC Data.frame to upload
#' @param qcName Name of the qc file
#' @import readr
#' @import googledrive
createNewQCSheet <- function(qcdf,qcName){
  readr::write_csv(qcdf,path = file.path(tempdir(),"qc.csv"))
  googledrive::drive_upload(name = qcName,media = file.path(tempdir(),"qc.csv"),type = "spreadsheet")
}




