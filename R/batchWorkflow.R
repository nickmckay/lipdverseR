
#' Read, standardize, and stage a directory of LiPD files for database ingestion
#'
#' Generalizes the hydro2kupdate.R workflow into a single callable function.
#' Reads LiPD files from inputDir, assigns datasetIds and changelogs, fixes
#' publication author lists, calls addLipdBatchToDatabase() to stage files in
#' the holding tank, then updates the datasetsInCompilation tab in the
#' compilation QC Google Sheet.
#'
#' After resolving any issues in the returned issues sheet, call
#' commitBatchToDatabase() to move files from the holding tank to the real
#' database.
#'
#' @param inputDir directory containing .lpd files to ingest
#' @param compilationName string written to paleoData_createdBy on new time series
#' @param dbPath path to the real database
#' @param holdingTankPath staging directory (default: ~/Dropbox/lipdverse/batchHoldingTank/)
#' @param googEmail Google account email for Sheets auth
#' @param issuesSheetId existing issues sheet to append to; NULL creates a new one
#' @param standardTables preloaded standardTables; loaded from web if NULL
#' @return invisibly: result from addLipdBatchToDatabase()
#' @export
prepareAndAddBatch <- function(inputDir,
                               compilationName,
                               dbPath = "/Users/nicholas/Dropbox/lipdverse/database/",
                               holdingTankPath = "~/Dropbox/lipdverse/batchHoldingTank/",
                               googEmail = "nick.mckay2@gmail.com",
                               issuesSheetId = NULL,
                               standardTables = NULL) {

  alp <- list.files(inputDir, recursive = TRUE, pattern = "\\.lpd$", full.names = TRUE)
  if (length(alp) == 0) stop(glue::glue("No .lpd files found in {inputDir}"))
  message(glue::glue("Found {length(alp)} .lpd file(s) in {inputDir}"))

  D <- lipdR::readLipd(alp)

  # Pre-processing: ensure datasetIds, changelogs, and clean author lists
  for (i in seq_along(D)) {
    L <- D[[i]]
    if (is.null(L$datasetId)) {
      L$datasetId <- createDatasetId()
    }
    if (is.null(L$changelog)) {
      L <- lipdR::initializeChangelog(L)
    }
    D[[i]] <- fixPubAuthorList(L)
  }

  if (is.null(standardTables)) {
    updateVocabWebsites()
    message("Loading standardTables from lipdverse.org...")
    standardTables <- getStandardTables()
  }

  result <- addLipdBatchToDatabase(
    D = D,
    dbPath = dbPath,
    holdingTankPath = holdingTankPath,
    googEmail = googEmail,
    issuesSheetId = issuesSheetId,
    createdBy = compilationName,
    standardize = TRUE,
    standardTables = standardTables
  )

  invisible(result)
}
