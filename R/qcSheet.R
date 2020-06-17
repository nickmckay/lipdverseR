#' Get the number of unique 14C ages in a Lipd file
#'
#' @param L
#' @param maxAge
#'
#' @return
#' @export
nUniqueAges <- function(L,maxAge = 12000,c14names = c("age")){
  ages <- c()
  if(length(L$chronData)>0){
    for(c in 1:length(L$chronData)){
      for(m in 1:length(L$chronData[[c]]$measurementTable)){
        #find which 14C column
        wc <- which(c14names %in% tolower(names(L$chronData[[c]]$measurementTable[[m]])))
        if(length(wc)>0){
          tc = min(wc)
          au <- L$chronData[[c]]$measurementTable[[m]][[c14names[tc]]]$units
          av <- L$chronData[[c]]$measurementTable[[m]][[c14names[tc]]]$values
          if(!is.null(au)){
            if(tolower(au)=="kyr"){
              av <- av*1000
            }
          }
          av <- av[av<=maxAge]
          ages <- c(ages,av[!is.na(av)])
        }
      }

    }
  }
  L$nUniqueAges <- length(unique(ages))
  return(L)
}

#' does the paleodata have depth?
#'
#' @param L
#' @param dnames names to use to look for depth
#'
#' @return
#' @export
hasDepth <- function(L,dnames = c("depth")){
  L$hasDepth <- 0
  if(length(L$paleoData)>0){
    for(c in 1:length(L$paleoData)){
      for(m in 1:length(L$paleoData[[c]]$measurementTable)){
        #find which depth column
        wc <- which(dnames %in% tolower(names(L$paleoData[[c]]$measurementTable[[m]])))
        if(length(wc)>0){
          tc = min(wc)
          av <- L$paleoData[[c]]$measurementTable[[m]][[dnames[tc]]]$values
          if(sum(is.finite(av))>0){
            L$hasDepth <- 1
            return(L)
          }
        }
      }

    }
  }
  return(L)
}

#' Get the number of good 14C ages in a Lipd file
#'
#' @param L
#' @param maxAge
#'
#' @return
#' @export
nGoodAges <- function(L,maxAge = 12000,c14names = c("age")){
  cts <- lipdR::extractTs(L,mode = "chron")

  if(length(cts)>0){#then there's a chron

    #fix kyr
    cts <- fixKiloyearsTsChron(cts)


    vn <- lipdR::pullTsVariable(cts,"chronData_variableName")
    ati <- which(vn == "age_type")
    if(length(ati)==0){
      out <- "no age_type column"
    }
    ai <- which(vn == "age")

    if(length(ai)==0){
      out <- "no age column"
    }

    a <- cts[[ai[1]]]$chronData_values

    #look for a column of 14C ages
    a14ci <- which(tolower(vn) == "age14c")
    if(length(a14ci)>0){
      #then combine with ages
      a14c <- cts[[a14ci[1]]]$chronData_values
      ba <- which(!is.finite(a))
      if(is.null(a)){#no "age" column
        a <- a14c
        out <- "no age column, but there is a 14C column"
      }
      a[ba] <- a14c[ba]
    }

    at <- cts[[ati[1]]]$chronData_values




    #find good age types
    ind <- which(grepl("14",at, ignore.case = TRUE) |
                   grepl("u/th",at, ignore.case = TRUE) |
                   grepl("tephra",at, ignore.case = TRUE)
    )



    goodAge <- a < maxAge
    goodAge[is.na(goodAge)] <- FALSE
    goodi <- which(goodAge)
    allGood <- intersect(ind,goodi)

    nGoodAges <- length(allGood)
    L$nUniqueGoodAges <- nGoodAges
    return(L)

  }else{
    return(L)
  }
}



#' Get the number of good 14C ages in a Lipd file
#'
#' @param L
#' @param maxAge
#'
#' @return
#' @export
nOtherAges <- function(L,maxAge = 12000,c14names = c("age")){
  cts <- lipdR::extractTs(L,mode = "chron")

  if(length(cts)>0){#then there's a chron
    vn <- lipdR::pullTsVariable(cts,"chronData_variableName")
    ati <- which(vn == "age_type")
    if(length(ati)==0){
      out <- "no age_type column"
    }
    ai <- which(vn == "age")
    if(length(ai)==0){
      out <- "no age column"
    }

    at <- cts[[ati[1]]]$chronData_values
    a <- cts[[ai[1]]]$chronData_values




    #find good age types
    ind <- which(!grepl("14c",at, ignore.case = TRUE) &
                   !grepl("c14",at, ignore.case = TRUE) &
                   !grepl("u/th",at, ignore.case = TRUE) &
                   !grepl("tephra",at, ignore.case = TRUE)
    )

    goodAge <- a < maxAge
    goodAge[is.na(goodAge)] <- FALSE
    goodi <- which(goodAge)
    allGood <- intersect(ind,goodi)

    nGoodAges <- length(allGood)
    L$nUniqueOtherAges <- nGoodAges
    return(L)

  }else{
    return(L)
  }
}


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
        if(is.null(names(tl))){
          tl <- tl[[1]]
        }

        if(length(tl) == 1){
          fnvec <- tl$name
          if(is.null(fnvec)){
            nvec[i] <- NA
          }else{
            nvec[i] <- tl$name
          }
        }else if(length(tl) > 1){
          nvec[i] <- paste(sapply(tl,"[[","name"),collapse = " ; ")
        }else{
          nvec[i] <- NA
        }

      }

    }
  }else if(class(vec)=="character"){
    nvec <- vec
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
  qcs <- readr::read_csv(here::here("googleQC.csv"),guess_max = Inf) %>%
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

updateFromQC <- function(sTS,qcs,compilationName = "test",newVersion = "0.0.0"){

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
  TSid <- lipdR::pullTsVariable(sTS,"paleoData_TSid")
  dsn <- lipdR::pullTsVariable(sTS,"dataSetName")
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

  #determine order to go through TS, want to go backwards through the QC sheet so apply2all changes at the top are applied last
  qcTSid <- as.character(qcs$TSid)
  extraTSid <- setdiff(TSid,qcTSid)

  TSidList <- c(extraTSid, qcTSid[rev(seq_along(qcTSid))])



  for(thisTSid in TSidList){
    i = which(TSid == thisTSid)
    if(length(i)>1){
      report = rbind(report,stringr::str_c("Too many matches for TSid: ",thisTSid ," for variableName: ",sTS[[i[1]]]$paleoData_variableName ," in dataset:",sTS[[i[1]]]$dataSetName) )
    }else if(length(i)<1){
      report = rbind(report,stringr::str_c("No matches for TSid in database: ",thisTSid) )
    }else{#then contunue
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
          if(length(rn) == 1){
            #check type
            varType <- convo$type[which(convo$tsName %in% thisTSnames[j])]
            if(varType == "character"){
              varFun <- as.character
            }else if(varType == "numeric"){
              varFun <- as.numeric
            }else if(varType == "inCompilation"){
              varFun <- as.logical
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
              if(varType == "inCompilation"){
                inThisComp <- varFun(qcs[qci,rn])

                if(isTRUE(inThisComp)){#then we need to add it to the compilation
                  #get all the names of the compilations
                  allComps <- thisTSnames[grepl(pattern = "inCompilationBeta[0-9]+_compilationName",thisTSnames)]
                  getCompNames <- function(x,y){y[[x]]}
                  compNames <- purrr::map_chr(allComps,getCompNames,newTS[[i]])
                  #and the numbers
                  compNum <- stringr::str_extract(allComps,pattern = "[0-9]+")
                  #see if the name matches any existing compilatinos
                  compInd <- which(compilationName == compNames)
                  if(length(compInd)==1){#1 match!
                    #what compilation number?
                    thisCompNum <- compNum[compInd]
                    #append this version
                    newTS[[i]][[str_c("inCompilationBeta",thisCompNum,"_compilationVersion")]] <- c(newTS[[i]][[str_c("inCompilationBeta",thisCompNum,"_compilationVersion")]],newVersion)
                  }else if(length(compInd)>1){#Oh no
                    stop("cant have two compilation matches")
                  }else{#must be a new compilation!
                    if(length(compNames)==0){#no comps in this TS yet
                      compNum <- 0
                      }
                    #what compilation number?
                    thisCompNum <- max(as.numeric(compNum))+1
                    #append this version
                    newTS[[i]][str_c("inCompilationBeta",thisCompNum,"_compilationName")] <- c(compilationName)
                    newTS[[i]][str_c("inCompilationBeta",thisCompNum,"_compilationVersion")] <- c(newVersion)
                  }
                }
              }else{#everything else
                newTS[[i]][thisTSnames[j]] <- varFun(qcs[qci,rn])

            }
            if(length(rn) > 1){
              stop("there shouldn't be multiple matches in ts names")
            }
          }
        }#end loop through variables and force an update
      }
      }
    }
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
createQCdataFrame <- function(sTS,templateId,to.omit = c("age","year"),to.omit.specific = c("depth","yr"),ageOrYear = "age",compilationName = NA,compVersion = NA){
  #setup reporting
  report <- c()
  noMatch <- c()
  #
  #download qc sheet template
  setwd(here::here())
  x <- googledrive::drive_get(googledrive::as_id(templateId))
  qc <- googledrive::drive_download(x,path = here::here("template.csv"),type = "csv",overwrite = T)
  qcs <- readr::read_csv(here::here("template.csv"),guess_max = Inf)

  #download name conversion
  convo <-  googledrive::as_id("1T5RrAtrk3RiWIUSyO0XTAa756k6ljiYjYpvP67Ngl_w") %>%
    googledrive::drive_get() %>%
    googledrive::drive_download(path = here::here("convo.csv"),overwrite = T) %>%
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
    toi <- append(toi,which(tolower(to) == tolower(uvn)))
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

  if(ageOrYear=="age"){

    #convert years, calculate min/max years
    toRep <- which(sapply(allAge,length)==0 & sapply(allYear,length)>0)

    for(t in toRep){
      allAge[[t]] <- convertAD2BP(allYear[[t]])
    }

    #climate12k specific

    #mean value
    getMean12k <- function(tsi){
      gi <- which(tsi$age<12000)
      return(mean(tsi$paleoData_values[gi],na.rm = TRUE))
    }

    getRes12k <- function(tsi){
      gi <- which(tsi$age<12000 & is.numeric(tsi$age) & !is.na(tsi$paleoData_values))
      if(length(gi)>=1){
        out <- median(abs(diff(sort(tsi$age[gi]))),na.rm = TRUE)
      }else{
        out <- NA
      }
      return(out)
    }

    meanValue <- purrr::map_dbl(fsTS,getMean12k)
    medianRes <- purrr::map_dbl(fsTS,getRes12k)

    fsTS <- pushTsVariable(fsTS,"paleoData_meanValue12k",vec = meanValue,createNew = TRUE)
    fsTS <- pushTsVariable(fsTS,"paleoData_medianRes12k",vec = medianRes,createNew = TRUE)


    #is annual
    annOpts <- c("1 2 3 4 5 6 7 8 9 10 11 12","ANN")
    ci1s <- try(pullTsVariable(fsTS,"climateInterpretation1_seasonality"))
    if(!class(ci1s)=="try-error"){
      ina <- which(is.na(ci1s))
      nci1s <- matrix(FALSE,nrow = length(ci1s) )
      nci1s[ina] <- NA
      isAnn <- which(ci1s %in% annOpts)
      nci1s[isAnn] <- TRUE
    }else{
      nci1s <- matrix(NA,nrow = length(fsTS) )
    }
    fsTS <- pushTsVariable(fsTS,"climateInterpretation1_isAnnual",nci1s,createNew = TRUE)


    #find NAs before
    allVals <- pullTsVariable(fsTS,"paleoData_values")

    goodfun <- function(age,vals,fun){
      out <- fun(age[is.finite(vals)],na.rm = TRUE)
    }

    minAge <- purrr::map2_dbl(allAge,allVals,goodfun,min)
    minAge[!is.finite(minAge)] <- NA

    maxAge <- purrr::map2_dbl(allAge,allVals,goodfun,max)
    maxAge[!is.finite(maxAge)] <- NA


    #     minAge <- sapply(allAge,min,na.rm=TRUE)
    #     maxAge <- sapply(allAge,max,na.rm=TRUE)

    #ages per kyr
    nUniqueGoodAges <- try(pullTsVariable(fsTS,"nUniqueGoodAges"))
    if(!class(nUniqueGoodAges)=="try-error"){
      maxHoloAge <- maxAge
      maxHoloAge[maxAge>12000] <- 12000
      agesPerKyr <- 1000*nUniqueGoodAges/(maxHoloAge-minAge)
    }else{
      agesPerKyr <- matrix(NA,nrow = length(fsTS) )
    }

    #other ages per kyr

    nUniqueOtherAges <- try(pullTsVariable(fsTS,"nUniqueOtherAges"))
    if(!class(nUniqueOtherAges)=="try-error"){
      maxHoloAge <- maxAge
      maxHoloAge[maxAge>12000] <- 12000
      otherAgesPerKyr <- 1000*nUniqueOtherAges/(maxHoloAge-minAge)
    }else{
      otherAgesPerKyr <- matrix(NA,nrow = length(fsTS) )
    }


    fsTS <- pushTsVariable(fsTS,"minYear",minAge,createNew = TRUE)
    fsTS <- pushTsVariable(fsTS,"maxYear",maxAge, createNew = TRUE)
    fsTS <- pushTsVariable(fsTS,"agesPerKyr",agesPerKyr,createNew = TRUE)
    fsTS <- pushTsVariable(fsTS,"otherAgesPerKyr",otherAgesPerKyr,createNew = TRUE)


  }else if(ageOrYear=="year"){
    #convert years, calculate min/max years
    toRep <- which(sapply(allYear,length)==0 & sapply(allAge,length)>0)

    for(t in toRep){
      allYear[[t]] <- convertBP2AD(allAge[[t]])
    }

    #has ages
    nUniqueAges <- try(pullTsVariable(fsTS,"nUniqueAges"))
    if(class(nUniqueAges)=="try-error"){
      nUniqueAges <- matrix(0,nrow = length(fsTS) )
    }

    hasChron <- as.numeric(nUniqueAges>0)
    fsTS <- pushTsVariable(fsTS,"hasChron",hasChron,createNew = TRUE)

    #has depth


    minYear <- sapply(allYear,min,na.rm=TRUE)
    maxYear <- sapply(allYear,max,na.rm=TRUE)
    fsTS <- pushTsVariable(fsTS,"minYear",minYear,createNew = TRUE)
    fsTS <- pushTsVariable(fsTS,"maxYear",maxYear, createNew = TRUE)
  }else{
    stop("ageOrYear must be 'age' or 'year'")
  }


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
    if(length(n2p) == 0){n2p <- "missingVariable!!!"}
    if(n2p == "inCompilationBeta_struct"){#figure out wheter it's in the compilation or not
        vec <- inThisCompilation(TS = fsTS,compName = compilationName,compVers = compVersion)
    }else if(any(n2p==allNames)){#regular check
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


#' In this compilation
#'
#' @param TS
#' @param compName
#' @param compVers
#'
#' @return
#' @export
#'
#' @examples
inThisCompilation <- function(TS,compName,compVers){
  allNames <- sort(unique(unlist(sapply(TS,names))))#get all names in TS
  #get all the names of the compilations
  allComps <- allNames[grepl(pattern = "inCompilationBeta[0-9]+_compilationName",allNames)]
  allVers <- allNames[grepl(pattern = "inCompilationBeta[0-9]+_compilationVersion",allNames)]

if(length(allComps) == 0){
 return(matrix(NA,nrow = length(TS)))

}
  allCompNames <- vector(mode = "list",length=length(allComps))
  allCompVersions <- vector(mode = "list",length=length(allComps))

  #get all the data
  for(i in 1:length(allComps)){
  allCompNames[[i]] <- pullTsVariable(TS,allComps[i])
  allCompVersions[[i]] <- pullTsVariable(TS,allVers[i])
  }

  #check to see if they match
  checkfun <- function(cn,cv,compName,compVers){
    bothMatch <- (cn==compName & purrr::map_lgl(cv,function(x){any(x == compVers)}))
    #put NAs back in for compName
    incn <- which(is.na(cn))
    bothMatch[incn] <- NA
    return(bothMatch)
  }

  #check for each compilation
  compCheck <- purrr::map2_dfc(allCompNames,allCompVersions,checkfun,compName,compVers)

  #check across rows
  unify <- as.matrix(apply(compCheck,1,any))

  return(unify)

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

#' Create the files for a new project
#' @export
#' @param qcdf QC Data.frame to upload
#' @param qcName Name of the qc file
#' @import readr googlesheets4 lubridate dplyr googledrive
createNewProject <- function(templateID = "1JEm791Nhd4fUuyqece51CSlbR2A2I-pf8B0kFgwynug",project = "newProject", versionMetaId = "1OHD7PXEQ_5Lq6GxtzYvPA76bpQvN1_eYoFR0X80FIrY"){

  #update the google version file
  versionDf <- googlesheets4::read_sheet(googledrive::as_id(versionMetaId))

  if(any(versionDf$project == project)){
    stop(paste("A project by the name",project,"already exists. Please provide a new name if this is a new project."))
  }


  versionDf$versionCreated <- lubridate::ymd_hms(versionDf$versionCreated)


  newRow <- versionDf[1,]

  newRow$project <- project
  newRow$publication <- 0
  newRow$dataset <- 0
  newRow$metadata <- 0
  newRow$dsns <- "NA"
  newRow$versionCreated <- lubridate::now(tzone = "UTC")
  newRow$`zip MD5` <- "startMD5"
  newRow$`dataSets removed` <- " "
  newRow$`dataSets added` <- " "
  nvdf <- dplyr::bind_rows(versionDf,newRow)
  readr::write_csv(nvdf,path = file.path(tempdir(),"versTemp.csv"))
  googledrive::drive_update(media = file.path(tempdir(),"versTemp.csv"),file = googledrive::as_id(versionMetaId),name = "lipdverse versioning spreadsheet")


  #copy the template file
  template <- getGoogleQCSheet(templateID)
  template <- template[1,]
  template[] <- NA

  newQc <- createNewQCSheet(template,qcName = project)
  newLastUpdate <- createNewQCSheet(template,qcName = paste(project,"last update"))



  return(dplyr::bind_rows(newQc,newLastUpdate))


}




