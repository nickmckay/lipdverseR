
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
                                      onTimeout = "error"),silent = FALSE)

    if(is(wrote,"try-error")){
      tries <- tries + 1
      message(glue::glue("write_sheet_retry: attempt {tries}/{ntries} failed."))
      Sys.sleep(min(tries * 2, 30))
    }else{
      break
    }

    if(tries > ntries){
      break
    }
  }

  if(is(wrote,"try-error")){
    stop(glue::glue("write_sheet_retry: failed after {ntries} attempts. Last error above."))
  }else{
    return(wrote)
  }
}



#' Append rows to a google sheet with retries
#'
#' @param data data.frame of rows to append
#' @param ss spreadsheet id
#' @param sheet sheet name or index
#' @param ntries number of tries
#' @param timeout seconds before timeout
#'
#' @return
#' @export
sheet_append_retry <- function(data, ss = NULL, sheet = NULL, ntries = 50, timeout = 60) {
  tries <- 0
  while (TRUE) {
    wrote <- try(R.utils::withTimeout(
      googlesheets4::sheet_append(ss = ss, data = data, sheet = sheet),
      timeout = timeout, onTimeout = "error"
    ), silent = FALSE)

    if (is(wrote, "try-error")) {
      tries <- tries + 1
      message(glue::glue("sheet_append_retry: attempt {tries}/{ntries} failed."))
      Sys.sleep(min(tries * 2, 30))
    } else {
      break
    }

    if (tries > ntries) break
  }

  if (is(wrote, "try-error")) {
    stop(glue::glue("sheet_append_retry: failed after {ntries} attempts."))
  } else {
    return(invisible(wrote))
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
read_sheet_retry <- function(ss = NULL, sheet = NULL, ntries = 20, timeout = 1200, guess_max = Inf, ...) {

  tries <- 0
  while(TRUE){
    read <- try(R.utils::withTimeout({googlesheets4::read_sheet(ss = ss, sheet = sheet, guess_max = guess_max, ...)},
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
