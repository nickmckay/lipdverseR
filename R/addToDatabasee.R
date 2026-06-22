
#' Ensure all variables in a LiPD object have TSids
#'
#' Walks paleoData and chronData measurement, summary, and ensemble tables and
#' creates TSids for any variable that is missing one. Generated TSids are
#' prefixed with "miss" and are checked for uniqueness against all existing
#' TSids in the object before assignment.
#'
#' @param L a LiPD object
#' @param usedTSids character vector of TSids already used outside this file
#'   (e.g. accumulated across a batch loop) to ensure cross-file uniqueness
#' @return list with elements L (updated LiPD object) and usedTSids (updated vector)
#' @export
ensureTSids <- function(L, usedTSids = character(0)) {

  # Collect all TSids already present in this file
  collectTSids <- function(table) {
    for (var in names(table)) {
      el <- table[[var]]
      if (is.list(el) && !is.null(el$variableName)) {
        if (!is.null(el$TSid) && !is.na(el$TSid) && el$TSid != "") {
          usedTSids <<- c(usedTSids, el$TSid)
        }
      }
    }
  }

  for (pd_i in seq_along(L$paleoData)) {
    for (mt_i in seq_along(L$paleoData[[pd_i]]$measurementTable))
      collectTSids(L$paleoData[[pd_i]]$measurementTable[[mt_i]])
    for (mod_i in seq_along(L$paleoData[[pd_i]]$model)) {
      for (tbl_i in seq_along(L$paleoData[[pd_i]]$model[[mod_i]]$summaryTable))
        collectTSids(L$paleoData[[pd_i]]$model[[mod_i]]$summaryTable[[tbl_i]])
      for (tbl_i in seq_along(L$paleoData[[pd_i]]$model[[mod_i]]$ensembleTable))
        collectTSids(L$paleoData[[pd_i]]$model[[mod_i]]$ensembleTable[[tbl_i]])
    }
  }
  for (cd_i in seq_along(L$chronData)) {
    for (mt_i in seq_along(L$chronData[[cd_i]]$measurementTable))
      collectTSids(L$chronData[[cd_i]]$measurementTable[[mt_i]])
  }

  # Generate a unique TSid with "miss" prefix, tracking all used IDs
  newUniqueTSid <- function() {
    repeat {
      id <- paste0("miss", lipdR::createTSid())
      if (!id %in% usedTSids) {
        usedTSids <<- c(usedTSids, id)
        return(id)
      }
    }
  }

  addMissingTSids <- function(table) {
    for (var in names(table)) {
      el <- table[[var]]
      if (is.list(el) && !is.null(el$variableName)) {
        if (is.null(el$TSid) || is.na(el$TSid) || el$TSid == "") {
          table[[var]]$TSid <- newUniqueTSid()
        }
      }
    }
    return(table)
  }

  for (pd_i in seq_along(L$paleoData)) {
    for (mt_i in seq_along(L$paleoData[[pd_i]]$measurementTable)) {
      L$paleoData[[pd_i]]$measurementTable[[mt_i]] <-
        addMissingTSids(L$paleoData[[pd_i]]$measurementTable[[mt_i]])
    }
    for (mod_i in seq_along(L$paleoData[[pd_i]]$model)) {
      for (tbl_i in seq_along(L$paleoData[[pd_i]]$model[[mod_i]]$summaryTable)) {
        L$paleoData[[pd_i]]$model[[mod_i]]$summaryTable[[tbl_i]] <-
          addMissingTSids(L$paleoData[[pd_i]]$model[[mod_i]]$summaryTable[[tbl_i]])
      }
      for (tbl_i in seq_along(L$paleoData[[pd_i]]$model[[mod_i]]$ensembleTable)) {
        L$paleoData[[pd_i]]$model[[mod_i]]$ensembleTable[[tbl_i]] <-
          addMissingTSids(L$paleoData[[pd_i]]$model[[mod_i]]$ensembleTable[[tbl_i]])
      }
    }
  }
  for (cd_i in seq_along(L$chronData)) {
    for (mt_i in seq_along(L$chronData[[cd_i]]$measurementTable)) {
      L$chronData[[cd_i]]$measurementTable[[mt_i]] <-
        addMissingTSids(L$chronData[[cd_i]]$measurementTable[[mt_i]])
    }
  }

  return(list(L = L, usedTSids = usedTSids))
}


#' create a reference table for the database
#'
#' @param D
#'
#' @return
#' @export
createDatabaseReference <- function(D){
  did <- purrr::map_chr(D,"datasetId")
  dsn <- purrr::map_chr(D,"dataSetName")
  ref <- tibble::tibble(datasetId = did, dataSetName = dsn)

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
                              dbRef = NA,
                              standardize = FALSE,
                              createdBy = NA,
                              parallelize = FALSE,
                              checkValid = TRUE){


  if(currentlyUpdating()){
    stop("Can't update any files because an update is currently running.\n\n Check https://lipdverse.org/updateStatus.txt for details")
  }

  #test for valid LiPD file
  if(checkValid){
    isValid <- lipdR::validLipd(L)

    if(!isValid){
      stop("The LiPD file is not valid. Run lipdR::validLipd() to diagnose the problems")
    }

  }
  if(all(is.na(dbRef))){
    if(exists("databaseRef",envir = .GlobalEnv)){
      databaseRef <- get("databaseRef",envir = .GlobalEnv)
    }else{
      databaseRef <- createDatabaseReference(lipdR::readLipd(dbPath,parallel = parallelize))
      assign("databaseRef",value = databaseRef,envir = .GlobalEnv)
    }
  }else{
    databaseRef <- dbRef
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
    cl <- lipdR::createChangelog(Lold,L)
    res <- "Updated"
    databaseRef <<- databaseRef

  }else if(L$dataSetName %in% databaseRef$dataSetName){#if the datasetId doesn't match, but the name does, we need to ask
    message(L$dataSetName)
    input <- geoChronR::askUser("This dataSetName is already present in the database, but with a different datasetId. Do you want to:\n

1. Change the new datasetId to match the old one and then update (usually a good idea)\n
2. Overwrite the old file in the database with the new one (usually a bad idea)\n
3. Change the dataSetName of the new file, if it's a legitimately new file (depends)\n
4. Abort\n")
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
    if(input == "3"){
      while(L$dataSetName %in% databaseRef$dataSetName){
        L$dataSetName <- geoChronR::askUser("What should the new dataSetName be?")
        if(L$dataSetName %in% databaseRef$dataSetName){
          message("The name still matches an existing one. Try again.")
        }
      }

      #looks like a new dataset, create a blank changelog
      cl <- createChangelog(L,L)
      res <- "Added"
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
    L <- lipdR::initializeChangelog(L,notes = "Added to lipdverse database.")
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

  if(all(!is.na(createdBy))){
    ts <- as.lipdTsTibble(L)
    if(!any(names(ts) == "paleoData_createdBy")){
      ts$paleoData_createdBy <- NA
    }
    newCols <- which(is.na(ts$paleoData_createdBy))
    ts$paleoData_createdBy[newCols] <- createdBy
    L <- as.lipd(ts)
  }

  lipdR::writeLipd(L,file.path(dbPath))

  print(glue::glue("{res} {L$dataSetName} ({L$datasetId})"))


}


#' Add a batch of LiPD files to the holding tank for review
#'
#' Processes a list of LiPD objects without any interactive prompts. All files
#' are written to a holding tank directory. Standardization issues and conflicts
#' are collected into a Google Sheet for human review. Call
#' commitBatchToDatabase() after resolving issues to move files into the real
#' database.
#'
#' @param D named list of LiPD objects
#' @param dbPath path to the real database (used only to build databaseRef)
#' @param holdingTankPath staging directory for processed files
#' @param googEmail Google account email for Sheets auth
#' @param issuesSheetId existing Google Sheet ID to append issues to; NULL creates a new sheet
#' @param createdBy optional string written to paleoData_createdBy for new time series
#' @param standardize run standardizeLipdBatch() on each file before staging
#' @param conflictResolution "matchId" (adopt existing datasetId) or "skip" (log and skip)
#' @param standardTables preloaded standardTables; loaded from web if NULL
#' @return invisibly: list(n_staged, n_skipped, issues_df, issuesSheetId, issuesSheetUrl, holdingTankPath)
#' @export
addLipdBatchToDatabase <- function(D,
                                   dbPath = "/Users/nicholas/Dropbox/lipdverse/database/",
                                   holdingTankPath = "~/Dropbox/lipdverse/batchHoldingTank/",
                                   googEmail = "nick.mckay2@gmail.com",
                                   issuesSheetId = NULL,
                                   createdBy = NA,
                                   standardize = TRUE,
                                   conflictResolution = "matchId",
                                   standardTables = NULL) {

  holdingTankPath <- path.expand(holdingTankPath)
  if (!dir.exists(holdingTankPath)) dir.create(holdingTankPath, recursive = TRUE)

  googledrive::drive_auth(email = googEmail, cache = ".secret")
  googlesheets4::gs4_auth(email = googEmail, cache = ".secret")

  if (standardize && is.null(standardTables)) {
    message("Loading standardTables from lipdverse.org...")
    standardTables <- getStandardTables()
  }

  # Build databaseRef once
  if (exists("databaseRef", envir = .GlobalEnv)) {
    databaseRef <- get("databaseRef", envir = .GlobalEnv)
  } else {
    message("Building database reference (reading database)...")
    databaseRef <- createDatabaseReference(lipdR::readLipd(dbPath))
    assign("databaseRef", value = databaseRef, envir = .GlobalEnv)
  }

  all_issues <- list()
  n_staged <- 0
  n_skipped <- 0
  usedTSids <- character(0)

  for (i in seq_along(D)) {
    L <- D[[i]]
    dsn <- if (is.null(L$dataSetName)) paste0("file_", i) else L$dataSetName

    # Fill missing TSids before validation, accumulating used IDs across files
    tsid_result <- ensureTSids(L, usedTSids)
    L <- tsid_result$L
    usedTSids <- tsid_result$usedTSids
    L_base <- L  # snapshot after TSid fixing; used as changelog baseline

    # Validation check
    if (!lipdR::validLipd(L)) {
      message(glue::glue("SKIP {dsn}: failed validation"))
      all_issues[[length(all_issues) + 1]] <- tibble::tibble(
        datasetId = if (is.null(L$datasetId)) NA_character_ else L$datasetId,
        dataSetName = dsn, TSid = NA_character_,
        issue_type = "validation_failed", field = NA_character_,
        current_value = NA_character_, suggested_value = NA_character_,
        add_synonym = NA_character_, new_term = NA_character_, past_match = NA_character_, past_id = NA_character_, resolution = NA_character_, status = ""
      )
      n_skipped <- n_skipped + 1
      next
    }

    # Ensure datasetId
    if (is.null(L$datasetId)) {
      L$datasetId <- lipdverseR::createDatasetId()
      while (L$datasetId %in% databaseRef$datasetId) {
        L$datasetId <- lipdverseR::createDatasetId()
      }
    }

    # Conflict detection
    res <- "Added"
    if (L$datasetId %in% databaseRef$datasetId) {
      res <- "Updated"
    } else if (dsn %in% databaseRef$dataSetName) {
      if (conflictResolution == "matchId") {
        wdsn <- which(databaseRef$dataSetName == dsn)
        L$datasetId <- databaseRef$datasetId[wdsn[1]]
        res <- "Updated (datasetId matched to existing)"
        all_issues[[length(all_issues) + 1]] <- tibble::tibble(
          datasetId = L$datasetId, dataSetName = dsn, TSid = NA_character_,
          issue_type = "name_id_conflict", field = NA_character_,
          current_value = NA_character_, suggested_value = NA_character_,
          add_synonym = NA_character_, new_term = NA_character_, past_match = NA_character_, past_id = NA_character_, resolution = "auto: adopted existing datasetId", status = "ok"
        )
      } else {
        message(glue::glue("SKIP {dsn}: name/id conflict and conflictResolution='skip'"))
        all_issues[[length(all_issues) + 1]] <- tibble::tibble(
          datasetId = L$datasetId, dataSetName = dsn, TSid = NA_character_,
          issue_type = "name_id_conflict", field = NA_character_,
          current_value = NA_character_, suggested_value = NA_character_,
          add_synonym = NA_character_, new_term = NA_character_, past_match = NA_character_, past_id = NA_character_, resolution = NA_character_, status = ""
        )
        n_skipped <- n_skipped + 1
        next
      }
    }

    # Standardization
    file_issues <- tibble::tibble(
      datasetId = character(), dataSetName = character(), TSid = character(),
      issue_type = character(), field = character(), current_value = character(),
      suggested_value = character(), add_synonym = character(), new_term = character(),
      past_match = character(), past_id = character(), resolution = character(), status = character()
    )
    if (standardize) {
      std_result <- standardizeLipdBatch(L, standardTables)
      L <- std_result$L
      file_issues <- std_result$issues
    }

    # Changelog — compare against TSid-fixed baseline, not the raw original
    if (is.null(L$changelog)) {
      L <- lipdR::initializeChangelog(L, notes = glue::glue("{res} in lipdverse database via batch workflow."))
    } else {
      cl <- createChangelog(L_base, L)
      L <- updateChangelog(L, changelog = cl,
                           notes = glue::glue("{res} in lipdverse database via batch workflow."))
    }

    # createdBy
    if (all(!is.na(createdBy))) {
      ts <- as.lipdTsTibble(L)
      if (!any(names(ts) == "paleoData_createdBy")) ts$paleoData_createdBy <- NA
      newCols <- which(is.na(ts$paleoData_createdBy))
      ts$paleoData_createdBy[newCols] <- createdBy
      L <- as.lipd(ts)
    }

    # Write to holding tank
    lipdR::writeLipd(L, holdingTankPath)
    message(glue::glue("Staged: {res} {dsn} ({L$datasetId})"))
    n_staged <- n_staged + 1

    # Track in databaseRef
    if (!L$datasetId %in% databaseRef$datasetId) {
      databaseRef <- dplyr::bind_rows(
        databaseRef,
        tibble::tibble(datasetId = L$datasetId, dataSetName = dsn)
      )
    }

    # File-level status row (ok if no issues)
    if (nrow(file_issues) == 0) {
      all_issues[[length(all_issues) + 1]] <- tibble::tibble(
        datasetId = L$datasetId, dataSetName = dsn, TSid = NA_character_,
        issue_type = "none", field = NA_character_,
        current_value = NA_character_, suggested_value = NA_character_,
        add_synonym = NA_character_, resolution = NA_character_, status = "ok"
      )
    } else {
      all_issues[[length(all_issues) + 1]] <- file_issues
    }
  }

  # Write issues sheet
  issues_df <- dplyr::bind_rows(all_issues)
  sheet_url <- NA_character_
  sheet_name <- glue::glue("lipdverse-batch-{Sys.Date()}")

  if (nrow(issues_df) > 0) {
    # Flatten any list-typed columns so sheet_write() accepts the data frame
    issues_to_write <- dplyr::mutate(
      issues_df,
      dplyr::across(dplyr::everything(), ~if (is.list(.)) purrr::map_chr(., ~paste(.x, collapse = "|")) else as.character(.))
    )

    if (is.null(issuesSheetId)) {
      new_ss <- googlesheets4::gs4_create(sheet_name)
      issuesSheetId <- as.character(new_ss)  # gs4_create() returns a sheets_id directly
      write_sheet_retry(data = issues_to_write, ss = issuesSheetId, sheet = 1)
      sheet_url <- glue::glue("https://docs.google.com/spreadsheets/d/{issuesSheetId}")
      message(glue::glue("Issues sheet created: {sheet_url}"))
    } else {
      write_sheet_retry(data = issues_to_write, ss = issuesSheetId, sheet = sheet_name)
      sheet_url <- glue::glue("https://docs.google.com/spreadsheets/d/{issuesSheetId}")
      message(glue::glue("Issues appended to sheet: {sheet_url}"))
    }
  }

  message(glue::glue(
    "\nBatch complete:\n  Staged: {n_staged}\n  Skipped: {n_skipped}\n  Issues: {nrow(dplyr::filter(issues_df, status == ''))}\n  Holding tank: {holdingTankPath}"
  ))

  invisible(list(
    n_staged = n_staged,
    n_skipped = n_skipped,
    issues_df = issues_df,
    issuesSheetId = issuesSheetId,
    issuesSheetUrl = sheet_url,
    holdingTankPath = holdingTankPath
  ))
}


# Internal: push synonym additions and new terms from the issues sheet to the
# appropriate vocabulary Google Sheets.  All changes for a given sheet are
# batched locally and written in a single call.
updateVocabSheetsFromIssues <- function(issues_df, googEmail = "nick.mckay2@gmail.com") {
  to_update <- dplyr::filter(
    issues_df,
    issue_type == "unknown_vocabulary",
    trimws(as.character(add_synonym)) == "TRUE" | trimws(as.character(new_term)) == "TRUE"
  )
  if (nrow(to_update) == 0) {
    message("No vocabulary sheet updates needed.")
    return(invisible(NULL))
  }

  # Normalize interpretation1_variable -> interpretation_variable etc.
  to_update <- dplyr::mutate(to_update,
    field_key = sub("^interpretation\\d+_", "interpretation_", field)
  )

  # One action per unique (field_key, current_value) — deduplicate across datasets
  to_update <- dplyr::distinct(to_update, field_key, current_value, .keep_all = TRUE)

  allKeys <- read_sheet_retry("16edAnvTQiWSQm49BLYn_TaqzHtKO9awzv5C-CemwyTY")

  for (fk in unique(to_update$field_key)) {
    key_row <- dplyr::filter(allKeys, name == fk)
    if (nrow(key_row) == 0) {
      warning(glue::glue("No vocab sheet found for field '{fk}', skipping."))
      next
    }

    ss_id    <- key_row[["googlesheets id"]][1]
    vocab_df <- read_sheet_retry(ss = ss_id, col_types = "c")
    field_rows <- dplyr::filter(to_update, field_key == fk)
    new_rows <- list()

    for (i in seq_len(nrow(field_rows))) {
      row <- field_rows[i, ]

      if (trimws(as.character(row$add_synonym)) == "TRUE") {
        # Synonym: copy the template row for suggested_value, replace synonym
        template <- dplyr::filter(vocab_df, lipdName == trimws(row$suggested_value))
        if (nrow(template) == 0) {
          warning(glue::glue(
            "add_synonym: '{row$suggested_value}' not found in {fk} sheet; skipping '{row$current_value}'."
          ))
          next
        }
        new_row          <- template[1, ]
        new_row$synonym  <- row$current_value
        new_rows[[length(new_rows) + 1]] <- new_row

      } else if (trimws(as.character(row$new_term)) == "TRUE") {
        # New term: suggested_value is the new lipdName; current_value is its synonym
        new_row              <- vocab_df[0, ][NA_integer_, ]
        new_row[]            <- NA_character_
        new_row$lipdName     <- trimws(row$suggested_value)
        new_row$synonym      <- row$current_value
        if ("paleoData_pastId" %in% names(new_row) &&
            !is.na(row$past_id) && trimws(row$past_id) != "") {
          new_row$paleoData_pastId <- trimws(row$past_id)
        }
        if ("paleoData_pastName" %in% names(new_row) &&
            !is.na(row$past_match) && trimws(row$past_match) != "") {
          new_row$paleoData_pastName <- trimws(row$past_match)
        }
        new_rows[[length(new_rows) + 1]] <- new_row
      }
    }

    if (length(new_rows) > 0) {
      updated_vocab <- dplyr::bind_rows(vocab_df, dplyr::bind_rows(new_rows))
      write_sheet_retry(data = updated_vocab, ss = ss_id, sheet = 1)
      message(glue::glue("Updated vocab sheet '{fk}': added {length(new_rows)} row(s)."))
    }
  }

  invisible(NULL)
}


# Internal: apply filled-in resolutions from issues sheet to a list of LiPD objects
applyIssueResolutions <- function(D_list, issues_df) {
  to_apply <- dplyr::filter(issues_df,
                             !is.na(resolution) & nchar(trimws(resolution)) > 0 &
                               issue_type == "unknown_vocabulary")

  if (nrow(to_apply) == 0) return(D_list)

  for (i in seq_len(nrow(to_apply))) {
    row <- to_apply[i, ]
    did <- row$datasetId
    matches <- which(purrr::map_chr(D_list, "datasetId") == did)
    if (length(matches) == 0) next

    L <- D_list[[matches[1]]]
    ts <- as.lipdTsTibble(L)

    if (!is.na(row$TSid) && row$TSid %in% ts$paleoData_TSid) {
      tidx <- which(ts$paleoData_TSid == row$TSid)
      if (row$field %in% names(ts)) {
        ts[[row$field]][tidx] <- row$resolution
      }
    } else if (is.na(row$TSid)) {
      # File-level field (e.g. archiveType)
      L[[row$field]] <- row$resolution
    }

    L <- as.lipd(ts)
    cl <- createChangelog(D_list[[matches[1]]], L)
    L <- updateChangelog(L, changelog = cl,
                         notes = glue::glue("Applied batch issue resolution for {row$field}"))
    D_list[[matches[1]]] <- L
  }

  return(D_list)
}


#' Commit a reviewed holding tank batch to the real database
#'
#' Reads all LiPD files from the holding tank, applies any resolutions filled
#' in on the issues sheet, then writes each file to the real database. Aborts
#' if any issues still have status = "" (unresolved).
#'
#' @param holdingTankPath path to the holding tank directory
#' @param dbPath path to the real database
#' @param issuesSheetId Google Sheet ID produced by addLipdBatchToDatabase(); NULL skips sheet checks
#' @param issuesSheet sheet name or index within the issues spreadsheet
#' @param qcSheetId Google Sheet ID for the compilation QC sheet to update datasetsInCompilation; NULL skips
#' @param googEmail Google account email for Sheets auth
#' @return invisibly: list(n_committed, issuesSheetId)
#' @export
commitBatchToDatabase <- function(holdingTankPath = "~/Dropbox/lipdverse/batchHoldingTank/",
                                  dbPath = "/Users/nicholas/Dropbox/lipdverse/database/",
                                  issuesSheetId = NULL,
                                  issuesSheet = 1,
                                  qcSheetId = NULL,
                                  googEmail = "nick.mckay2@gmail.com") {

  holdingTankPath <- path.expand(holdingTankPath)

  if (!is.null(issuesSheetId)) {
    googledrive::drive_auth(email = googEmail, cache = ".secret")
    googlesheets4::gs4_auth(email = googEmail, cache = ".secret")
    issues_df <- read_sheet_retry(ss = issuesSheetId, sheet = issuesSheet)
    issues_df <- dplyr::mutate(issues_df, dplyr::across(dplyr::everything(), as.character))
    sheet_url <- glue::glue("https://docs.google.com/spreadsheets/d/{issuesSheetId}")

    # --- Validate unknown_vocabulary resolutions ---
    vocab_issues <- dplyr::filter(issues_df, issue_type == "unknown_vocabulary")
    if (nrow(vocab_issues) > 0) {
      add_syn_val  <- trimws(as.character(vocab_issues$add_synonym))
      add_syn_set  <- add_syn_val %in% c("TRUE", "FALSE")  # either value counts as resolved
      new_trm      <- trimws(as.character(vocab_issues$new_term)) == "TRUE"
      both         <- add_syn_val == "TRUE" & new_trm        # add_synonym=TRUE + new_term=TRUE contradicts
      neither      <- !add_syn_set & !new_trm                # nothing resolved yet
      # suggested_value is required in all resolved cases
      no_sugg      <- (add_syn_set | new_trm) & (is.na(vocab_issues$suggested_value) | trimws(vocab_issues$suggested_value) == "")

      bad <- dplyr::bind_rows(
        if (any(both))    dplyr::mutate(vocab_issues[both, ],    .problem = "both add_synonym=TRUE and new_term=TRUE") else NULL,
        if (any(neither)) dplyr::mutate(vocab_issues[neither, ], .problem = "neither add_synonym nor new_term is TRUE") else NULL,
        if (any(no_sugg)) dplyr::mutate(vocab_issues[no_sugg, ], .problem = "suggested_value is blank") else NULL
      )
      if (nrow(bad) > 0) {
        stop(glue::glue(
          "Cannot commit: {nrow(bad)} unknown_vocabulary row(s) have invalid resolutions.\n",
          "Problem(s): {paste(unique(bad$.problem), collapse = '; ')}\n",
          "Dataset(s): {paste(unique(bad$dataSetName), collapse = ', ')}\n",
          "{sheet_url}"
        ))
      }
    }

    # --- Abort on any other unresolved non-vocabulary issues ---
    unresolved <- dplyr::filter(
      issues_df,
      issue_type != "none",
      issue_type != "name_id_conflict",
      issue_type != "unknown_vocabulary",
      is.na(status) | trimws(status) == ""
    )
    if (nrow(unresolved) > 0) {
      stop(glue::glue(
        "Cannot commit: {nrow(unresolved)} unresolved non-vocabulary issue(s) remain.\n",
        "Dataset(s): {paste(unique(unresolved$dataSetName), collapse = ', ')}\n",
        "{sheet_url}"
      ))
    }

    # --- Update vocabulary sheets (one write per sheet) ---
    message("Updating vocabulary sheets...")
    updateVocabSheetsFromIssues(issues_df, googEmail)

    # --- Auto-populate resolution = suggested_value for all resolved vocab rows ---
    # Covers add_synonym=TRUE/FALSE (field update + optional vocab write) and
    # new_term=TRUE (new lipdName added to vocab; LiPD field also updated).
    for (i in seq_len(nrow(issues_df))) {
      if (identical(issues_df$issue_type[i], "unknown_vocabulary") &&
          (trimws(as.character(issues_df$add_synonym[i])) %in% c("TRUE", "FALSE") ||
           trimws(as.character(issues_df$new_term[i])) == "TRUE") &&
          (is.na(issues_df$resolution[i]) || trimws(issues_df$resolution[i]) == "")) {
        issues_df$resolution[i] <- issues_df$suggested_value[i]
      }
    }

  } else {
    issues_df <- NULL
  }

  message("Reading files from holding tank...")
  D_list <- lipdR::readLipd(holdingTankPath)

  if (!is.null(issues_df)) {
    message("Applying resolutions...")
    D_list <- applyIssueResolutions(D_list, issues_df)
  }

  n_committed <- 0
  for (i in seq_along(D_list)) {
    L <- D_list[[i]]
    lipdR::writeLipd(L, dbPath)
    message(glue::glue("Committed: {L$dataSetName} ({L$datasetId})"))
    n_committed <- n_committed + 1
  }

  # Mark all rows committed in sheet
  if (!is.null(issuesSheetId) && !is.null(issues_df)) {
    issues_df$status <- "committed"
    write_sheet_retry(data = issues_df, ss = issuesSheetId, sheet = issuesSheet)
  }

  # Update datasetsInCompilation in the compilation QC sheet
  if (!is.null(qcSheetId)) {
    dsn  <- purrr::map_chr(D_list, "dataSetName")
    dsid <- purrr::map_chr(D_list, "datasetId")
    dbir <- read_sheet_retry(ss = qcSheetId, sheet = "datasetsInCompilation")
    tbt <- which(dbir$dsn %in% dsn)
    if (length(tbt) > 0) dbir$inComp[tbt] <- "TRUE"
    ta <- which(!dsn %in% dbir$dsn)
    if (length(ta) > 0) {
      nd <- data.frame(dsn = dsn[ta], dsid = dsid[ta], inComp = "TRUE", instructions = NA)
      dbir <- dplyr::bind_rows(dbir, nd)
    }
    write_sheet_retry(dbir, ss = qcSheetId, sheet = "datasetsInCompilation")
    message(glue::glue("Updated datasetsInCompilation in QC sheet ({qcSheetId})"))
  }

  # --- Final verification: confirm each holding-tank file is now in the database ---
  staged_names <- purrr::map_chr(D_list, "dataSetName")
  db_files     <- tools::file_path_sans_ext(basename(list.files(dbPath, pattern = "\\.lpd$")))
  confirmed    <- staged_names[staged_names %in% db_files]
  missing      <- staged_names[!staged_names %in% db_files]

  msg <- glue::glue("\nCommit complete: {length(confirmed)} of {length(staged_names)} file(s) verified in database.")
  if (length(missing) > 0) {
    msg <- paste0(msg, "\nNOT found in database after commit:\n",
                  paste0("  - ", missing, collapse = "\n"))
  } else {
    msg <- paste0(msg, "\nAll files confirmed present.")
  }
  message(msg)

  invisible(list(n_committed = n_committed, n_verified = length(confirmed),
                 missing = missing, issuesSheetId = issuesSheetId))
}
