
#' create a reference table for the database
#'
#' @param D
#'
#' @return
#' @export
createDatabaseReference <- function(D){
  lts <- lipdR::extractTs(D)
  did <- lipdR::pullTsVariable(lts,"datasetId",strict.search = TRUE)
  dsn <- lipdR::pullTsVariable(lts,"dataSetName",strict.search = TRUE)
  ref <- tibble::tibble(datasetId = did, dataSetName = dsn) %>%
    dplyr::distinct()

  if(any(duplicated(ref$datasetId))){
    id <- which(duplicated(ref$datasetId))
    for(idi in id){
      print(ref$dataSetName[ref$datasetId == ref$datasetId[idi]])
    }
    stop(glue::glue("Oh no. There are duplicated datasetIds in the database"))
  }
  if(any(duplicated(ref$dataSetNames))){
    warning("Hmmm. There are duplicated dataSetNames in the database. This probably shouldn't happen.")
  }

  return(ref)
}

#' Add or update a LiPD file in the database
#'
#' @param L
#' @param dbPath local path to the database
#' @export
addLipdToDatabase <- function(L,
                              dbPath = "/Users/nicholas/Dropbox/lipdverse/database/",
                              standardize = FALSE){


  if(currentlyUpdating()){
    stop("Can't update any files because an update is currently running.\n\n Check https://lipdverse.org/updateStatus.txt for details")
  }

  if(exists("databaseRef",envir = .GlobalEnv)){
    databaseRef <- get("databaseRef",envir = .GlobalEnv)
  }else{
    databaseRef <- createDatabaseReference(lipdR::readLipd(dbPath))
    assign("databaseRef",value = databaseRef,envir = .GlobalEnv)

  }

  #check for needed variables
  if(is.null(L$datasetId)){
    print(glue::glue("{L$dataSetName} is missing a datasetId. Generating one now."))
    L$datasetId <- lipdverseR::createDatasetId()
    createdNewDatasetId <- TRUE
  }else{
    createdNewDatasetId <- FALSE
  }


  #make sure that it's not creating a duplicat datasetId randomly somehow
  if(createdNewDatasetId){
    while(L$datasetId %in% databaseRef$datasetId){
      L$datasetId <- lipdverseR::createDatasetId()
    }
  }

  #see if datasetId exists
  if(L$datasetId %in% databaseRef$datasetId){
    #it's already there!
    dsn <- databaseRef$dataSetName[which(databaseRef$datasetId %in% L$datasetId)][1]
    Lold <- lipdR::readLipd(file.path(dbPath,paste0(dsn,".lpd")))
    cl <- createChangelog(Lold,L)
    res <- "Updated"
    databaseRef <<- databaseRef

  }else if(L$dataSetName %in% databaseRef$dataSetName){#if the datasetId doesn't match, but the name does, we need to ask
    input <- geoChronR::askUser("This dataSetName is already present in the database, but with a different datasetId. Do you want to:\n

1. Change the new datasetId to match the old one and then update (usually a good idea)\n
2. Overwrite the old file in the database with the new one (usually a bad idea)\n
3. Abort\n")
    if(input == "1"){
      wdsn <- which(databaseRef$dataSetName == L$dataSetName)
      if(length(wdsn) > 1){
        stop("multiple matches. This is bad.")
      }
      L$datasetId <- databaseRef$datasetId[wdsn]
      print(glue::glue("Updating datasetId for {L$dataSetName}, now: {L$datasetId}"))
      dsn <- databaseRef$dataSetName[which(databaseRef$datasetId %in% L$datasetId)][1]
      Lold <- lipdR::readLipd(file.path(dbPath,paste0(dsn,".lpd")))
      cl <- createChangelog(Lold,L)
      res <- "Updated (after updating datasetId to match)"
    }
    if(input == "2"){
      wdsn <- which(databaseRef$dataSetName == L$dataSetName)
      if(length(wdsn) > 1){
        stop("multiple matches. This is bad.")
      }
      print("Overwriting old file")
      #looks like a new dataset, create a blank changelog
      cl <- createChangelog(L,L)
      res <- "Added (after removing old entry and dataset Id)"

      #remove old instance from databaseRef
      databaseRef <- databaseRef[-wdsn,]

      #add to databaseRef

      databaseRef <- dplyr::bind_rows(databaseRef,tibble::tibble(datasetId = L$datasetId, dataSetName = L$dataSetName))
      databaseRef <<- databaseRef
    }

  }else{
    #looks like a new dataset, create a blank changelog
    cl <- createChangelog(L,L)
    res <- "Added"
    #add to databaseRef
    databaseRef <- dplyr::bind_rows(databaseRef,tibble::tibble(datasetId = L$datasetId, dataSetName = L$dataSetName))
    databaseRef <<- databaseRef
  }

  #update the changelog
  if(is.null(L$changelog)){
    print(glue::glue("{L$dataSetName} is missing a changelog. Intiating one now."))
    L <- lipdverseR::initializeChangelog(L,notes = "Added to lipdverse database.")
    if(res != "Added"){#update the changelog
      L <- updateChangelog(L,changelog = cl,notes = "Updated lipdverse database entry with a changed file.")
    }
  }else{#update the change log
    if(nrow(cl)==0){
      L <- updateChangelog(L,changelog = cl,notes = "Added to lipdverse database as a new dataset.")
    }else{
      L <- updateChangelog(L,changelog = cl,notes = "Updated lipdverse database entry with a changed file.")
    }
  }


  if(standardize){
    TS <- extractTs(L)
    isValidAll(TS)
  }

  lipdR::writeLipd(L,file.path(dbPath))

  print(glue::glue("{res} {L$dataSetName} ({L$datasetId})"))


}
