removeRoothLevelVariablesWithUnderscores <- function(L){
  nl <- names(L)
  if(any(str_detect(pattern = "_",nl))){
    td <- which(str_detect(pattern = "_",nl))
    Lo <- L
    L[td] <- NULL
    cl <- createChangelog(Lo,L)
    L <- updateChangelog(L,cl)
    writeLipd(L,"~/Dropbox/lipdverse/database/")
  }
}


#' Remove all-NA columns from all tables in a LiPD object
#'
#' Walks every measurementTable, summaryTable, and ensembleTable in both
#' paleoData and chronData and drops any column-level list whose \code{values}
#' are entirely \code{NA}.
#'
#' @param L a LiPD object
#' @return the LiPD object with all-NA columns removed
#' @export
removeAllNaColumns <- function(L) {

  isAllNa <- function(col) {
    is.list(col) && !is.null(col$values) && all(is.na(col$values))
  }

  cleanTable <- function(tbl) {
    is_col <- purrr::map_lgl(tbl, is.list)
    for (nm in names(tbl)[is_col]) {
      if (isAllNa(tbl[[nm]])) {
        tbl[[nm]] <- NULL
      }
    }
    tbl
  }

  for (pc in c("paleoData", "chronData")) {
    if (length(L[[pc]]) == 0) next
    for (ni in seq_along(L[[pc]])) {
      for (mi in seq_along(L[[pc]][[ni]]$measurementTable)) {
        L[[pc]][[ni]]$measurementTable[[mi]] <-
          cleanTable(L[[pc]][[ni]]$measurementTable[[mi]])
      }
      for (mod_i in seq_along(L[[pc]][[ni]]$model)) {
        for (ti in seq_along(L[[pc]][[ni]]$model[[mod_i]]$summaryTable)) {
          L[[pc]][[ni]]$model[[mod_i]]$summaryTable[[ti]] <-
            cleanTable(L[[pc]][[ni]]$model[[mod_i]]$summaryTable[[ti]])
        }
        for (ti in seq_along(L[[pc]][[ni]]$model[[mod_i]]$ensembleTable)) {
          L[[pc]][[ni]]$model[[mod_i]]$ensembleTable[[ti]] <-
            cleanTable(L[[pc]][[ni]]$model[[mod_i]]$ensembleTable[[ti]])
        }
      }
    }
  }

  return(L)
}


#' Remove empty measurementTables from a LiPD object
#'
#' Removes any measurementTable that contains no column-level lists (i.e. no
#' variables) from both paleoData and chronData. Useful after
#' \code{removeAllNaColumns()} may have left behind empty tables.
#'
#' @param L a LiPD object
#' @return the LiPD object with empty measurementTables removed
#' @export
removeEmptyMeasurementTables <- function(L) {

  hasNoColumns <- function(tbl) {
    !any(purrr::map_lgl(tbl, is.list))
  }

  pcEntryIsEmpty <- function(entry) {
    length(entry$measurementTable) == 0 &&
      length(entry$model) == 0
  }

  for (pc in c("paleoData", "chronData")) {
    if (length(L[[pc]]) == 0) next

    for (ni in seq_along(L[[pc]])) {
      mt <- L[[pc]][[ni]]$measurementTable
      if (length(mt) == 0) next
      keep <- !purrr::map_lgl(mt, hasNoColumns)
      L[[pc]][[ni]]$measurementTable <- if (any(keep)) mt[keep] else NULL
    }

    # Drop entries that now have no tables of any kind
    keep_entries <- !purrr::map_lgl(L[[pc]], pcEntryIsEmpty)
    L[[pc]] <- L[[pc]][keep_entries]

    # Drop the entire paleoData/chronData list if nothing remains
    if (length(L[[pc]]) == 0) {
      L[[pc]] <- NULL
    }
  }

  return(L)
}
