#' create a simple page that lists files in the directory
#'
#' @param thisDir what directory to index?
#' @param webDirectory where's the main webdirectory
#'
#' @return writes an index html and Rmd fil
#' @export
createFileIndex <- function(thisDir,webDirectory = "~/Dropbox/lipdverse/html/"){

#get a directory of all files and create links
af <- list.files(thisDir,recursive = TRUE)

af <- af[!stringr::str_detect(af,"index")]

md <- c()

#load in the starter text
thisRmd <- readr::read_file(file.path(webDirectory,"startSimple.Rmd"))

#replace the title
thisRmd <- stringr::str_replace(thisRmd,pattern = "LiPD-Dashboards",replacement = basename(thisDir))


for(link in af){
  thisRmd <- thisRmd %>%
    stringr::str_c(glue::glue("[{link}]({link})")) %>%
    stringr::str_c("\n\n")
}

readr::write_file(thisRmd,path = file.path(thisDir,"index.Rmd"))
rmarkdown::render(file.path(thisDir,"index.Rmd"))

}
