

getCompInfo <- function(L,comp,compVers){

  ts <- extractTs(L)
  ic <- inThisCompilation(ts,comp,compVers)
  icnn <- na.omit(ic)
  if(length(icnn) == 0){
    itc <- "NA"
  }else if(any(icnn)){
    itc <- "TRUE"
  }else{
    itc <- "FALSE"
  }
  return(data.frame(dsn = L$dataSetName, dsid = L$datasetId,inComp = itc))
}

updateDatasetCompilationQc <- function(D,
                                       comp,
                                       compVers,
                                       qcSheetId){

  dscomp <- purrr::map_dfr(D,getCompInfo,comp,compVers) %>%
    dplyr::arrange(dsn)


  #compare with existing sheet, make sure datasetIds online are included

  googDsComp <- read_sheet_retry(ss = qcSheetId, sheet = "datasetsInCompilation",col_types = "c")

  missing <- dplyr::filter(googDsComp, ! dsid %in% dscomp$dsid)

  #also check for local files not on the list?

  dscomp <- dplyr::bind_rows(dscomp,missing) %>% dplyr::arrange(dsn)

  #make sure that all that were marked TRUE previously are still marked true. This is to prevent new datasets from being removed from this list before the curator has the chance to add to compilation. It also means that you can only remove datasets by marking them FALSE.
  it <- googDsComp$dsid[googDsComp$inComp == "TRUE"]
  tbt <- which(dscomp$dsid  %in% it)
  dscomp$inComp[tbt] <- "TRUE"


  dscomp$instructions <- ""

  dscomp$instructions[1] <- 'Any datasets marked as FALSE will not be considered for the update, NA or TRUE will be considered.'


  write_sheet_retry(dscomp, ss = qcSheetId, sheet = "datasetsInCompilation")


}

getDatasetInCompilationFromQC <- function(qcId = "1tYuhgaDPx1HxdSneL0Nl1Aq7LIM14jzbn5Ke55ha_z0"){
  qc <- read_sheet_retry(ss =  qcId,sheet = 1)
  if(any(names(qc) == "datasetId")){#we can do more
    stop("you have to code this")
  }else{
    dsnincomp <- read_sheet_retry(ss =  qcId,sheet = 2)

    datasetsToInclude <- qc %>%
      dplyr::filter(inThisCompilation != "FALSE") %>%
      dplyr::select(dataSetName) %>%
      unique() %>%
      unlist()

    good <- which(dsnincomp$dsn %in% datasetsToInclude)
    dsnincomp$inComp <- FALSE
    dsnincomp$inComp[good] <- TRUE

    write_sheet_retry(dsnincomp, ss = qcId, sheet = "datasetsInCompilation")
  }
}


