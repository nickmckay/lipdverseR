#' hasChron
#'
#' @param L
#'
#' @return
#' @export
#'
#' @examples
hasChron <- function(L){
  if(any(names(L)=="chronData")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


#' has chron ensemble
#'
#' @param L
#'
#' @return
#' @export
#'
#' @examples
hasChronEnsemble <- function(L){
  hce <- FALSE

  if(any(names(L)=="chronData")){
    for(i in 1:length(L$chronData)){
      if(any(names(L$chronData[[i]])=="model")){
        for(j in 1:length(L$chronData[[i]]$model)){
          if(any(names(L$chronData[[i]]$model[[j]])=="ensembleTable")){
            hce <- TRUE
            return(hce)
          }
        }
      }
    }
  }
  return(hce)
}

#' rename primary chronology
#'
#' @param L
#' @param avar
#'
#' @return
#' @export
#'
#' @examples
renamePrimaryChron <- function(L,avar ="age"){
  for(p in 1:length(L$paleoData)){
    for(m in 1:length(L$paleoData[[p]]$measurementTable)){
      isPrimary <- NA
      an <- names(L$paleoData[[p]]$measurementTable[[m]])
      wav <- which(stringr::str_starts(an,avar))
      if(length(wav >= 2)){
        for(a in wav){
          if(!is.null(L$paleoData[[p]]$measurementTable[[m]][[a]]$primaryAgeColumn)){
            if(L$paleoData[[p]]$measurementTable[[m]][[a]]$primaryAgeColumn == TRUE){
              isPrimary <-  a
            }
          }
        }
        if(!is.na(isPrimary)){#then rename it
          if(an[isPrimary] != avar){
            toChange <- setdiff(wav,isPrimary)
            for(t in 1:length(toChange)){
              an[toChange[t]] <- paste0(an[toChange[t]],"-",t)
            }
            an[isPrimary] <- avar
          }
          names(L$paleoData[[p]]$measurementTable[[m]]) <- an
        }
      }
    }
  }
  return(L)
}


