

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

  googDsComp <- googlesheets4::read_sheet(ss = qcSheetId, sheet = "datasetsInCompilation")

  missing <- dplyr::filter(googDsComp, ! dsid %in% dscomp$dsid)

  dscomp <- dplyr::bind_rows(dscomp,missing) %>% dplyr::arrange(dsn)


  dscomp$instructions <- ""

  dscomp$instructions[1] <- 'Any datasets marked as FALSE will not be considered for the update, NA or TRUE will be considered.'

  tries <- 0
  while(TRUE){
    wrote <- try(R.utils::withTimeout({googlesheets4::sheet_write(dscomp, ss = qcSheetId, sheet = "datasetsInCompilation")},
                                      timeout = 15,
                                      onTimeout = "error"),silent = TRUE)

    if(class(wrote) == "try-error"){
      tries <- tries + 1
    }else{
      break
    }

    if(tries > 20){
      break
    }
  }



}
