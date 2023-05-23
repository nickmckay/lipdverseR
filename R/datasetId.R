#' update dataset ID database
#'
#' @param D
#' @param compilation
#' @param version
#' @param dateUpdated
#'
#' @return a string describing the result
#' @export
updateDatasetIdDereferencer <- function(D,
                                        compilation,
                                        version,
                                        dateUpdated = lubridate::today()){

  #assign most recent version
  D <- purrr::map(D,writeVersionToRoot)

  TS <- extractTs(D)

  itc <- inThisCompilation(TS,compilation,version)
  gTS <- TS[which(itc)]

  #get variables
  dsid <- pullTsVariable(gTS,"datasetId",strict.search = TRUE)
  dsn <- pullTsVariable(gTS,"dataSetName",strict.search = TRUE)
  url <- pullTsVariable(gTS,"lipdverseLink",strict.search = TRUE)
  dsvers <- pullTsVariable(gTS,"datasetVersion",strict.search = TRUE)
  dstimestamp <- pullTsVariable(gTS,"datasetTimestamp",strict.search = TRUE)


  info <- data.frame(datasetId = dsid,
                         dataSetName = dsn,
                         datasetVersion = dsvers,
                         datasetTimestamp = dstimestamp,
                          url = url,
                         compilation = compilation,
                         version = version,
                         dateUpdated = dateUpdated) %>%
    dplyr::distinct()

#get google sheet
  drms <- read_sheet_retry("1zIaVN-zbuK6YqOvbiwLdLnFRh6QC9ljvDq9qB_gwB-8")

  ol <- nrow(drms)

  #append update
  ndrms <- dplyr::bind_rows(drms,info) %>% dplyr::distinct()

    nl <- nrow(ndrms)

  if(nl>ol){

    #update
    googlesheets4::write_sheet(data = ndrms,ss = "1zIaVN-zbuK6YqOvbiwLdLnFRh6QC9ljvDq9qB_gwB-8",sheet = 1)
    return(glue::glue("added {nl-ol} new entries to the dereferencer database"))
  }else{
    return("no new updates, nothing changed")
  }

}


