
#' write google sheet with many attempts
#'
#' @param data data.frame
#' @param ss ss
#' @param sheet sheet
#' @param ntries number of tries
#' @param timeout seconds before timeout. NA estimates from size.
#'
#' @return
#' @export
#'
#' @examples
write_sheet_retry <- function(data,ss = NULL,sheet = NULL,ntries = 50,timeout = NA){
  if(is.na(timeout)){
    timeout <- max(c(object.size(data)/1e6,20))
  }

  tries <- 0
  while(TRUE){
    wrote <- try(R.utils::withTimeout({googlesheets4::sheet_write(data, ss = ss, sheet = sheet)},
                                      timeout = timeout,
                                      onTimeout = "error"),silent = TRUE)

    if(is(wrote,"try-error")){
      tries <- tries + 1
    }else{
      break
    }

    if(tries > ntries){
      break
    }
  }

 # wrote <- purrr::insistently(googlesheets4::sheet_write(data, ss = ss, sheet = sheet))

  if(is(wrote,"try-error")){
    return("failed to write")
  }else{
    return(wrote)
  }


}



#' write google sheet with many attempts
#'
#' @param data data.frame
#' @param ss ss
#' @param sheet sheet
#' @param ntries number of tries
#' @param timeout seconds before timeout. NA estimates from size.
#'
#' @return
#' @export
read_sheet_retry <- function(ss = NULL,sheet = NULL,ntries = 20,timeout = 1200,...){

  tries <- 0
  while(TRUE){
    read <- try(R.utils::withTimeout({googlesheets4::read_sheet(ss = ss, sheet = sheet,...)},
                                      timeout = timeout,
                                      onTimeout = "error"),silent = TRUE)

    if(is(read,"try-error")){
      tries <- tries + 1
    }else{
      break
    }

    if(tries > ntries){
      break
    }
  }

  # wrote <- purrr::insistently(googlesheets4::sheet_write(data, ss = ss, sheet = sheet))

  if(is(read,"try-error")){
    stop(glue::glue("failed to read despit {ntries} tries"))
  }else{
    return(read)
  }


}
