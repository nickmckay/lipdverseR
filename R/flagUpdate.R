#' Turn on update flag
#'
#' @param compilation what compilation is updating?
#'
#' @return updates status website
#' @export
flagUpdate <- function(compilation){
  topost <- stringr::str_c("Update running\n", paste(compilation," - update started:",lubridate::now()))
  tf <- tempfile(fileext = ".txt")
  readr::write_file(topost,file = tf)
  system(glue::glue("scp {tf} npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/updateStatus.txt"))
}

#' Turn off update flag
#'
#' @return updates status website
#' @export
unFlagUpdate <- function(){
  topost <- stringr::str_c("No update is currently running\n", paste("Status as of:",lubridate::now()))
  tf <- tempfile(fileext = ".txt")
  readr::write_file(topost,file = tf)
  system(glue::glue("scp {tf} npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/updateStatus.txt"))
}


#' Is an update currently running?
#'
#' @return Boolean
#' @export
currentlyUpdating <- function(){
  uf <- readr::read_file("https://lipdverse.org/updateStatus.txt")
  notRunning <- grepl(x = uf,pattern = "No update is currently running",fixed = TRUE)
  isRunning <- grepl(x = uf,pattern = "Update running",fixed = TRUE)

  if(notRunning & !isRunning){#checks out, not running
    return(FALSE)
  }else if(!notRunning & isRunning){#checks out, is running
    return(TRUE)
  }else{
    stop("Something is not right when checking whether an update is running")
  }


}
