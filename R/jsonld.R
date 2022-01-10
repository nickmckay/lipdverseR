#' Remove json-ld tage from html
#'
#' @param dataSetName
#' @param webdir
#'
#' @return
#' @export
removeJsonldFromHtml <- function(dataSetName,webdir){
  #check for lpd and html files

  lpdpath <- file.path(webdir,paste0(dataSetName,'.lpd'))
  htmlpath <- file.path(webdir,paste0(dataSetName,".html"))
  if(!file.exists(htmlpath)){
    stop(glue::glue("{htmlpath} is missing, make sure it's been generated"))
  }
  if(!file.exists(lpdpath)){
    stop(glue::glue("{lpdpath} is missing, make sure it's been generated"))
  }


h <- readLines(htmlpath)

#find </head>


#check for header
if(!any(str_detect(h,fixed('<script type="application/ld+json">')))){
  return("Does not have a json ld tag")
}

hs <- stringr::str_locate(h,fixed('<script type="application/ld+json">'))

he <- stringr::str_locate(h,fixed('</script>'))

hsg <- min(which(!is.na(hs[,1])))
heg <- min(which(!is.na(he[,1])))


if(is.finite(hsg) & is.finite(heg)){
  tt <- h[-(hsg:heg)]
  writeLines(text = tt,con = htmlpath)
  return(paste("Removed jsonld header from",htmlpath))
}else{
  return("no json ld header found")
}
}

createSitemapXml <- function(webdir,
                             torep = "/Users/nicholas/Dropbox/lipdverse/html",
                             repwith = "http://lipdverse.org",
                             outfile = file.path(torep,"sitemap.xml") ){
  html_full <- list.files(path = webdir,pattern = "*.html",full.names = TRUE)
  dates <- lubridate::ymd(lubridate::as_date(file.mtime(html_full)))
  html <- stringr::str_replace_all(html_full, pattern = torep,replacement = repwith)

  uf <- function(loc,lastmod){
  url <- list(loc = loc,lastmod = lastmod)
  return(list(url = url))
  }

  urlset <- purrr::map2(html,dates,uf) %>%
    listToXml("urlset") %>%
    XML::saveXML(file = outfile)

  smo <- readLines(outfile)
  smo[1] <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  smo[2] <- '<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">'


  oi <- which(stringr::str_detect(smo,"<>") | stringr::str_detect(smo,"</>"))

  writeLines(smo[-oi],outfile)

}


##' Convert List to XML
##'
##' Can convert list or other object to an xml object using xmlNode
##' @title List to XML
##' @param item
##' @param tag xml tag
##' @return xmlNode
##' @export
##' @author David LeBauer, Carl Davidson, Rob Kooper
listToXml <- function(item, tag) {
  # just a textnode, or empty node with attributes
  if(typeof(item) != 'list') {
    if (length(item) > 1) {
      xml <- xmlNode(tag)
      for (name in names(item)) {
        xmlAttrs(xml)[[name]] <- item[[name]]
      }
      return(xml)
    } else {
      return(xmlNode(tag, item))
    }
  }

  # create the node
  if (identical(names(item), c("text", ".attrs"))) {
    # special case a node with text and attributes
    xml <- xmlNode(tag, item[['text']])
  } else {
    # node with child nodes
    xml <- xmlNode(tag)
    for(i in 1:length(item)) {
     # if (names(item)[i] != ".attrs") {
        xml <- append.xmlNode(xml, listToXml(item[[i]], names(item)[i]))
     # }
    }
  }

  # add attributes to node
  attrs <- item[['.attrs']]
  for (name in names(attrs)) {
    xmlAttrs(xml)[[name]] <- attrs[[name]]
  }
  return(xml)
}

addJsonldToDir <- function(webdir,overwrite = FALSE){

  ajq <- function(...){
    try(addJsonldToHtml(...),silent = TRUE)
    }
  list.files(path = webdir,pattern = "*.lpd") %>%
    tools::file_path_sans_ext() %>%
purrr::walk(ajq,webdir,overwrite = overwrite)
}


addJsonldToHtml <- function(dataSetName,webdir, overwrite = FALSE){
  #check for lpd and html files

  lpdpath <- file.path(webdir,paste0(dataSetName,'.lpd'))
  htmlpath <- file.path(webdir,paste0(dataSetName,".html"))
  if(!file.exists(htmlpath)){
    stop(glue::glue("{htmlpath} is missing, make sure it's been generated"))
  }
  if(!file.exists(lpdpath)){
    stop(glue::glue("{lpdpath} is missing, make sure it's been generated"))
  }

  #create snippet
  L <- lipdR::readLipd(lpdpath)
  j <- createJsonldSnippet(L)

  jh <- stringr::str_c('<script type="application/ld+json">\n') %>%
    stringr::str_c(j) %>%
    stringr::str_c('\n </script>')



  #add html buffer info

  h <- readLines(htmlpath)

  #find </head>

  hl <- stringr::str_locate(h,"<head>")

  #check for header
  if(any(str_detect(h,fixed('<script type="application/ld+json">')))){
    if(overwrite){
      removeJsonldFromHtml(dataSetName,webdir)
      #repeat with updated file
      h <- readLines(htmlpath)
      hl <- stringr::str_locate(h,"<head>")

    }else{
            return("Did nothing, since this file already has a tag. Set `overwrite = TRUE` to overwrite the old tag")
    }
  }

  hr <- min(which(!is.na(hl[,1])))

  if(is.finite(hr)){
    tt <- c(h[1:hr],jh,h[-c(1:hr)])
    writeLines(text = tt,con = htmlpath)
    return(paste("Added jsonld header to",htmlpath))
  }else{
    return("no <head> found")
  }

}

addThroughputWidgetToHtml <- function(dataSetName,webdir, overwrite = FALSE){
  #check for lpd and html files

  lpdpath <- file.path(webdir,paste0(dataSetName,'.lpd'))
  htmlpath <- file.path(webdir,paste0(dataSetName,".html"))
  if(!file.exists(htmlpath)){
    stop(glue::glue("{htmlpath} is missing, make sure it's been generated"))
  }
  if(!file.exists(lpdpath)){
    stop(glue::glue("{lpdpath} is missing, make sure it's been generated"))
  }

  #create snippet
  L <- lipdR::readLipd(lpdpath)
  t <- createThroughputWidget(L)

  header <- '<script type="module" src="https://unpkg.com/throughput-widget/dist/throughputwidget/throughputwidget.esm.js"></script>\n'



  #add html buffer info

  h <- readLines(htmlpath)

  #find </head>

  hl <- stringr::str_locate(h,"<head>")

  #check for header
  if(any(str_detect(h,fixed('<script type="module" src="https://unpkg.com/throughput-widget/dist/throughputwidget/')))){
    if(overwrite){
      stop("not set up yet")
      removeThoughputFromHtml(dataSetName,webdir)
      #repeat with updated file
      h <- readLines(htmlpath)
      hl <- stringr::str_locate(h,"<head>")

    }else{
      return("Did nothing, since this file already has a tag. Set `overwrite = TRUE` to overwrite the old tag")
    }
  }

  hr <- min(which(!is.na(hl[,1])))

  if(is.finite(hr)){
    tt <- c(h[1:hr],header,h[-c(1:hr)])
  }else{
    return("no <head> found")
  }

  #now write the widget itself
  hl2 <- stringr::str_locate(tt,'<div id="row" class="section level2 tabset tabset-fade">')

  #check for header
  if(any(str_detect(h,fixed('<throughput-widget identifier')))){
    if(overwrite){
      stop("not set up yet")
      removeThoughputFromHtml(dataSetName,webdir)
      #repeat with updated file
      h <- readLines(htmlpath)
      hl <- stringr::str_locate(h,"<head>")

    }else{
      return("Did nothing, since this file already has a widget. Set `overwrite = TRUE` to overwrite the old tag")
    }
  }


  hr2 <- min(which(!is.na(hl2[,1]))) - 1

  if(is.finite(hr2)){
    tt <- c(tt[1:hr2],t,tt[-c(1:hr2)])
  }else{
    return('no <div id="row" class="section level2 tabset tabset-fade"> found')
  }

  writeLines(text = tt,con = htmlpath)
  return(paste("Added throughput widget header to",htmlpath))

}

createThroughputWidget <- function(L){
  th <-  "<throughput-widget " %>%
  str_c('identifier="r3d100012894" ') %>%
  str_c(glue::glue('link="',L$datasetId,'" ')) %>%
  str_c('additional-type="http://linked.earth/ontology%23Dataset" ') %>%
  str_c('orcid-client-id="APP-W2LVD1XEIP1Z2Z9N" ') %>%
  str_c('</throughput-widget>')

  return(th)
}

createJsonldSnippet <- function(L){
  J <- list()
  #context
  J[["@context"]] <- list()
  J[["@context"]][["@vocab"]] <- "https://schema.org/"

  #type
  J[["@type"]] <- "Dataset"

  #additionalType
  J$additionalType <- "http://linked.earth/ontology/core/1.2.0/index-en.html#Dataset"


  if(is.null(L$datasetId)){stop("Dataset must have a datasetId")}
  #id
  #J["@id"] <- glue::glue("metadata:{L$datasetId}")
  J[["@id"]] <- L$lipdverseLink

  #identifier
  J[["identifier"]] <- list()
  J[["identifier"]][["@id"]] <- glue::glue("https://lipdverse.org/resource/dataset/{L$datasetId}")
  J[["identifier"]][["@type"]] <- "PropertyValue"
  J[["identifier"]][["propertyID"]] <- "http://linked.earth/ontology#hasDatasetId"
  J[["identifier"]][["name"]] <- glue::glue("lipdverse dataset ID {L$datasetId}")
  J[["identifier"]][["value"]] <- L$datasetId
  J[["identifier"]][["url"]] <- glue::glue("https://lipdverse.org/resource/dataset/{L$datasetId}")


  #name
  J$name <- L$dataSetName

  #description
  J$description <- createDescription(L)

  #distribution
  J$distribution <- vector(mode = "list",length = 1)
  J$distribution[[1]]$`@type` <- "DataDownload"
  J$distribution[[1]]$contentUrl <- stringr::str_replace(L$lipdverseLink,".html",".lpd")
  J$distribution[[1]]$encodingFormat <- c("application/zip", "http://linked.earth/ontology/core/1.2.0/index-en.html#Dataset")
  J$distribution[[1]]$datePublished <- parsedate::format_iso_8601(Sys.time())
    as.character()

  #url
  J$url <- L$lipdverseLink

  #version
  J$version <- getVersion(L)

  #keywords #variable names for now
  J$keywords <- extractTs(L) %>%
    pullTsVariable("paleoData_variableName") %>%
    setdiff(c("age","depth","year")) %>%
    as.list()

  #add geospatial information
  J$spatialCoverage[["@type"]] <- "Place"
  if(!is.null(L$geo$siteName)){
    J$spatialCoverage[["name"]] <- L$geo$siteName
  }

  J$spatialCoverage$geo <- list()
  J$spatialCoverage[["geo"]][["@type"]] <- "GeoCoordinates"
  J$spatialCoverage[["geo"]][["latitude"]] <- L$geo$latitude
  J$spatialCoverage[["geo"]][["longitude"]] <- L$geo$longitude
  if(!is.null(L$geo$elevation)){
    if(!is.na(L$geo$elevation)){
      J$spatialCoverage[["geo"]][["elevation"]] <- L$geo$elevation
    }
  }
  if(!is.null(L$geo$description)){
    if(!is.na(L$geo$description)){
      J$spatialCoverage[["geo"]][["description"]] <- L$geo$description
    }
  }


  #license
  J$license <- "CC-BY-4.0"


#write to json
json <- RJSONIO::toJSON(x = J)

return(json)
}


createDescription <- function(L){
  #https://developers.google.com/search/docs/data-types/dataset#dataset

  #current goal. Automate description using 1) Location, 2) Duration 3) variables and 4) archive type

  #location
  sn <- L$geo$siteName
  gcmd <- L$geo$gcmdLocation

  if(is.null(sn)){
    noLoc <- TRUE
  }else{
    noLoc <- FALSE
  }

  if(is.null(gcmd)){
    locstring <- sn
  }else{
    locstring <- glue::glue("{sn} ({gcmd})")
  }

  #duration
  ts <- extractTs(L)
  ages <- try(pullTsVariable(ts,"age"),silent = TRUE)
  years <- try(pullTsVariable(ts,"year"),silent = TRUE)
  au <- try(pullTsVariable(ts,"ageUnits"),silent = TRUE)
  yu <- try(pullTsVariable(ts,"yearUnits"),silent = TRUE)

  noDur <- FALSE
  if(class(ages) != "try-error"){#lets go
    if(is.list(ages)){
      young <- min(sapply(ages,min,na.rm = TRUE))
      old <- max(sapply(ages,max,na.rm = TRUE))
    }else{
      young <- min(ages,na.rm = TRUE)
      old <- max(ages,na.rm = TRUE)
    }
    units <- unique(au)[1]
  }else if(class(years) != "try-error"){
    if(is.list(years)){
      young <- max(sapply(ages,max,na.rm = TRUE))
      old <- min(sapply(ages,min,na.rm = TRUE))
    }else{
      young <- max(years,na.rm = TRUE)
      old <- min(years,na.rm = TRUE)
    }
    units <- unique(yu)[1]
  }else{
    noDur <- TRUE
  }

  if(!noDur){
    durstring <- glue::glue("from {round(old)} to {round(young)} ({units})")
  }

  #variables
  variables <- extractTs(L) %>%
    pullTsVariable("paleoData_variableName") %>%
    setdiff(c("age","depth","year"))

  if(length(variables) > 2){
  variables <- paste(variables,collapse= ", ") %>%
    stringr::str_replace(pattern = "(?=[^,]*$)",replacement = " and")
  }else if(length(variables) == 2){
    variables <- paste(variables,collapse= ", ") %>%
      stringr::str_replace(pattern = ",",replacement = " and")
  }


  if(noLoc){
    descstring <- glue::glue("This dataset is derived from a {L$archiveType} archive, and includes data on {variables}. The data are relevant to the time interval {durstring}.")
  }else{
    descstring <- glue::glue("This dataset from {locstring} is derived from a {L$archiveType} archive, and includes data on {variables}. The data are relevant to the time interval {durstring}.")
  }

return(descstring)

}



