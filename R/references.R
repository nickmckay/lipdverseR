goodEntry <- function(entry){
  good <- TRUE
  if(length(entry) == 0){
    good <- FALSE
    return(good)
  }
  if(is.null(entry)){
    good <- FALSE
    return(good)
  }
  if(is.na(entry)){
    good <- FALSE
    return(good)

  }
  if(is.character(entry)){
      if(grepl("^\\s*$",entry)){
        good <- FALSE
        return(good)
      }
  }
  return(good)
}

createBibtexEntry <- function(pub){
  #first check for DOI
  bibEntry <- try(suppressMessages(RefManageR::GetBibEntryWithDOI(pub$doi,delete.file = TRUE)),silent = TRUE)


  if(!is(bibEntry,"BibEntry") | length(bibEntry) == 0){
      pub$author <- formatAuthorForBibtex(pub)


    pubdf <- as.data.frame(pub)
    if(!"journal" %in% names(pubdf)){
      pubdf$journal <- "unknown"
    }

    if(!"bibtype" %in% names(pubdf)){
      if(!"type" %in% names(pubdf)){
      pubdf$bibtype <- "Article"
      }else{
        if(!grepl(pubdf$type[1],pattern = "article",ignore.case = TRUE)){
          pubdf$bibtype <- "misc"
        }else{
          pubdf$bibtype <- "Article"
        }
      }
    }

    if(!goodEntry(pubdf$author)){
      pubdf$author <- "Missing Author"
    }

    if(!goodEntry(pubdf$title)){
      pubdf$title <- "Missing Title"
    }

    if(!goodEntry(pubdf$year)){
      pubdf$year <- "Missing Year"
    }

    bibEntry <- RefManageR::as.BibEntry(pubdf)
  }

  #update the bib key
  titleForKey <- stringr::str_remove_all(bibEntry$title,pattern = "[^A-Za-z\\d]") %>%
    tolower()
  authorForKey <- RefManageR:::authorList(bibEntry)[1] %>% stringr::str_remove_all(pattern = "[^A-Za-z]") %>%
    tolower()
  yearForKey <- bibEntry$year
  lipdBibKey <- paste0(authorForKey,yearForKey,titleForKey) %>% stringr::str_trunc(127,side = "center",ellipsis = "")
  names(bibEntry) <- lipdBibKey

  return(bibEntry)
}

getDatasetBibtex <- function(L){
  databib <- purrr::map(L$pub,createBibtexEntry)
  return(databib)
}

#add new citekeys
updateGoogleReferencesFromLipd <- function(allRefTib){
  gs <- read_sheet_retry("1MPLsg7OLMMm5L2UV829OaXbK5B9zx6ng9cXWtwTZwgg")


  newRefs <- select(allRefTib,citekey,everything(),-citation,-datasetId,-dataSetName) %>%
    distinct() %>%
    filter(!startsWith(citekey,prefix = "author")) %>% #no missing authors
    filter(is.finite(as.numeric(year))) %>% #no missing years
    filter(!is.finite(as.numeric(citekey))) %>% #no only numeric citekeys
    filter(!grepl(citekey,pattern =  "needsatitle")) %>% #no needs a title
    filter(!duplicated(citekey)) %>% #no duplicates
    filter(!citekey %in% gs$citekey) #it's new



  if(any(duplicated(gs$citekey))){
    stop("there are duplicate citekeys, please fix in the google sheet and try again")
  }
  if(nrow(newRefs) > 0){
    forGoogle <- bind_rows(gs,newRefs)
    googlesheets4::gs4_auth(email = "nick.mckay2@gmail.com")
    write_sheet_retry(ss = "1MPLsg7OLMMm5L2UV829OaXbK5B9zx6ng9cXWtwTZwgg",data = forGoogle,sheet = "bib database")
    message(glue::glue("{nrow(newRefs)} new citekeys added to bibliographic database."))
    createLipdverseBibtexFromGoogle()
  }


}


createLipdverseBibtexFromGoogle <- function(path = "~/Dropbox/lipdverse/html/lipdverse/lipdverse.bib"){
  #get googlesheet
  gs <- read_sheet_retry("1MPLsg7OLMMm5L2UV829OaXbK5B9zx6ng9cXWtwTZwgg")
  class(gs) <- "data.frame"

  if(any(duplicated(gs$citekey))){
    stop("there are duplicate citekeys, please fix in the google sheet and try again")
  }

  base::rownames(gs) <- gs$citekey

  gs <- dplyr::select(gs,-citekey)

  bib <- RefManageR::as.BibEntry(gs)

  RefManageR::WriteBib(bib,file = path)

  system("scp /Users/nicholas/Dropbox/lipdverse/html/lipdverse/lipdverse.bib npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/lipdverse/lipdverse.bib")

}

createBibDfFromLipd <- function(D){

  giant <- purrr::map(D,getDatasetBibtex,.progress = TRUE)

  allRefTib <- as.data.frame(giant[[1]][1])
  allRefTib$dataSetName <- names(giant)[1]

  allDsn <- map_chr(D,"dataSetName")
  allDsid <- map_chr(D,"datasetId")

  for(i in 1:length(giant)){
    cat(glue("{round(100*i/length(giant))}%\r"))
    for(j in 1:length(giant[[i]])){
      newdf <- try(as.data.frame(giant[[i]][j]),silent = TRUE)
      if(!is(newdf,"try-error")){
        newdf$dataSetName <- names(giant)[i]
        newdf$citekey <- rownames(newdf)

        ii <- which(newdf$dataSetName[1] == allDsn)
        if(length(ii) == 1){
          newdf$datasetId <- as.character(allDsid[ii])
        }else{
          stop("This is bad")
        }
        allRefTib <-  dplyr::bind_rows(allRefTib,newdf)
      }
    }
  }

  return(allRefTib)

}

#forGoogle <- select(allRefTib,citekey,bibtype:urldate,-datasetId,-dataSetName, -citation) %>% distinct()

#googlesheets4::write_sheet(ss = "1MPLsg7OLMMm5L2UV829OaXbK5B9zx6ng9cXWtwTZwgg",data = forGoogle,sheet = "bib database")

createJsonDsidBibReference <- function(allRefTib,path = "~/Dropbox/lipdverse/html/lipdverse/bibDsid.json"){

  uDsid <- unique(allRefTib$datasetId)

  #create list of citekeys
  dsidCitekey <- list()
  for(u in uDsid){
    thisDsid <- dplyr::filter(allRefTib,datasetId == u)
    dsidCitekey[[u]]$key <- thisDsid$citekey
  }

  dsidCitekey[1] <- NULL
  j <- jsonlite::toJSON(dsidCitekey, pretty=TRUE, auto_unbox = TRUE)
  write(j, file=path)
  system("scp /Users/nicholas/Dropbox/lipdverse/html/lipdverse/bibDsid.json npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/lipdverse/bibDsid.json")
}

updateJsonDsidBibReference <- function(allRefTib,path = "~/Dropbox/lipdverse/html/lipdverse/bibDsid.json"){
  dsidCitekey <- jsonlite::read_json(path)

  uDsid <- unique(allRefTib$datasetId)

  for(u in uDsid){
    thisDsid <- dplyr::filter(allRefTib,datasetId == u)
    theseKeys <- thisDsid$citekey
    oldKeys <- unlist(dsidCitekey[[u]]$key)

    dsidCitekey[[u]]$key <- unique(c(theseKeys,oldKeys))
  }


  j <- jsonlite::toJSON(dsidCitekey, pretty=TRUE, auto_unbox = TRUE)
  write(j, file=path)


  system("scp /Users/nicholas/Dropbox/lipdverse/html/lipdverse/bibDsid.json npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/lipdverse/bibDsid.json")

}

getLipdverseBib <- function(get.new = FALSE){
  if(exists("lipdverseBibliography",envir = .GlobalEnv) & !get.new){
    LB <- get("lipdverseBibliography",envir = .GlobalEnv)
  }else{
    print("Downloading Lipdverse Bibliographic metadata. This will take about 30 seconds the first time you run it in each session.")
    download.file("https://lipdverse.org/lipdverse/lipdverse.bib",destfile = file.path(tempdir(),"lipdverse.bib"))
    LB <- RefManageR::ReadBib(file.path(tempdir(),"lipdverse.bib"))
    assign("lipdverseBibliography",value = LB,envir = .GlobalEnv)

  }
  return(LB)
}

getJsonBib <- function(){
    json <- jsonlite::read_json("https://lipdverse.org/lipdverse/bibDsid.json")
    return(json)
}

#' Create a RefManageR Bibentry for all the references in a multiLipd object
#'
#' @param D multilipd object
#'
#' @return RefManageR Bibentry object
#' @export
createBib <- function(D){
  #get lipdverse bib
  LB <- getLipdverseBib()

  #get lipdverse json reference
  jsonbib <- getJsonBib()

  #get all DSids
  dsids <- purrr::map_chr(D,"datasetId")
  citekeys <- getCiteKeyFromDsid(dsids,jsonbib)

  return(LB[citekeys])
}

formatAuthorForBibtex <- function(pub){
  if(is.list(pub$author)){
    pub$author <- purrr::map_chr(pub$author,purrr::possibly(\(x) x$name,otherwise = "Missing Author")) %>%
      paste(collapse = " and ")
  }

  if(is.null(pub$author)){
    pub$author <- "Missing Author"
  }

  if(is.na(pub$author)){
    pub$author <- "Missing Author"
  }

  step1 <- pub$author %>%
    str_replace_all(pattern = "\\., ",replacement = " and ") %>%
    str_replace_all(pattern = "; ",replacement = " and ") %>%
    str_replace_all(pattern = ";",replacement = " and ") %>%
    #look for commas without trailing spaces
    str_replace_all(pattern = ",([?!\\w])",replacement = ", \\1") %>%
    str_replace_all(pattern = " , ",replacement = " and ")

  step2 <- try(RefManageR:::ArrangeAuthors(step1),silent = TRUE)

  if(is(step2,"try-error")){
    pub$author <- step1
  }else{
    pub$author <- step2 %>%
      paste(collapse = " and ")
  }

  if(pub$author == ""){
    pub$author <- "Missing Author"
  }

  if(is.character(pub$author)){
    return(pub$author)
  }else{
    return("Missing Author")
  }
}

getAuthor <- function(L){
  allAuthors <- purrr::map_chr(L$pub,formatAuthorForBibtex)
  return(allAuthors)
}

getCiteKeyFromDsid <- function(dsid,json,bib,onlyIncludeThoseInBib = FALSE){
  gn <- which(names(json) %in% dsid)

  cks <- as.character(unlist(json[gn]))

  if(onlyIncludeThoseInBib){
    allInBib <- names(bib)
    good <- which(cks %in% allInBib)
    cks <- cks[good]
  }
  return(cks)
}



createBibliographicReferenceHtml <- function(DC,tsC,proj,projVersion,webdir = "/Volumes/data/Dropbox/lipdverse/html"){

  projDir <- file.path(webdir,proj,projVersion)

  #create a bibliography for this project
  thisBib <- createBib(DC)

  #write it to the project folder
  RefManageR::WriteBib(thisBib,file = file.path(projDir,glue::glue("{proj}-{projVersion}.bib")))

  #check for datasetVersion
  if(!"datasetVersion" %in% names(tsC)){
    DCm <- DC[tsC$dataSetName]
    allVersion <- map_chr(DCm,getVersion)
    tsC$datasetVersion <- allVersion
  }

  #create the table
  refTable <- createBibTable(ts = tsC,smallBib = thisBib)

  #save the table to the project directory
  saveRDS(refTable,file.path(projDir,glue::glue("{proj}-{projVersion}-refTable.RDS")))

  #prepare the quarto file
  quartoTemplate <- read_file(file.path(webdir,"bibtableTemplate.qmd"))

  #replace strings appropriately
  thisQuarto <- quartoTemplate %>%
    stringr::str_replace_all("Compilation",proj) %>%
    stringr::str_replace_all("Version",projVersion)

  readr::write_file(thisQuarto,file = file.path(projDir,glue::glue("references.qmd")))
  #cwd <- getwd()
  #setwd(projDir)
  quarto::quarto_render(file.path(projDir,glue::glue("references.qmd")),
                        output_format = "all")
  #setwd(cwd)

}

createBibTable <- function(ts,smallBib){

  #  mD <- D[ts$dataSetName]
  #  ts$datasetVersion <- map_chr(mD,getVersion)

  bibjson <- getJsonBib()

  smallTable <- ts %>%
    group_by(datasetId) %>%
    summarize(dataSetName = unique(dataSetName),
              datasetVersion = unique(datasetVersion),
              Lat = unique(geo_latitude),
              Lon = unique(geo_longitude),
              archiveType = unique(archiveType),
              proxy = paste(unique(na.omit(paleoData_proxy)),collapse = ", ")) %>%
    dplyr::rowwise() %>%
    mutate(dataSetName = paste0("[",dataSetName,"]","(https://lipdverse.org/data/",datasetId,"/",stringr::str_replace_all(datasetVersion,pattern = "[//.]","_"),"/)",collapse = ""),
           citations =  paste0("",map_chr(datasetId,\(x) paste0("@",getCiteKeyFromDsid(dsid = x,json = bibjson,bib = smallBib,onlyIncludeThoseInBib = TRUE), collapse = ";  ")),"")) %>%
    select(-datasetId) %>%
    arrange(dataSetName)

  empty <- which(smallTable$citations == "@")
  if(length(empty) > 0){
    smallTable$citations[empty] <- "Missing citation metadata"
  }

  return(smallTable)
}

createProjectBibPages <- function(D,proj,projVersion,webdir = "/Volumes/data/Dropbox/lipdverse/html"){
  TS <- extractTs(D)
  dsn <- pullTsVariable(TS,"dataSetName",strict.search = TRUE)
  itc <- inThisCompilation(TS, compName = proj,compVers = projVersion)
  g <- which(itc)
  gdsn <- unique(dsn[g])
  DC <- D[gdsn]
  tsC <- TS[g] %>% ts2tibble()

  createBibliographicReferenceHtml(DC,tsC,proj,projVersion,webdir)

}

#use this if you have new dois from the google sheet in a csv
updateGoogleReferencesFromCsv <- function(csv){
  gs <- read_sheet_retry("1MPLsg7OLMMm5L2UV829OaXbK5B9zx6ng9cXWtwTZwgg")


  #create a new reference tib for all the datasets in DOI

  df <- createBibDfFromCsv(csv)

  #get only the citekeys that we're not updating

  ntu <- filter(gs,!citekey %in% df$citekey)


  new <- bind_rows(ntu,df) %>%
    arrange(citekey)

  if(nrow(new) != nrow(gs)){
    stop("the number of rows should not be changing")
  }


  if(any(duplicated(new$citekey))){
    stop("there are duplicate citekeys, please fix in the google sheet and try again")
  }

  googlesheets4::gs4_auth(email = "nick.mckay2@gmail.com")
  write_sheet_retry(ss = "1MPLsg7OLMMm5L2UV829OaXbK5B9zx6ng9cXWtwTZwgg",data = new,sheet = "bib database")
  message(glue::glue("{nrow(df)} citekeys have been updated in the bibliographic database."))
  createLipdverseBibtexFromGoogle()

}


createBibDfFromCsv <- function(csv){
  bibEntry <- purrr::map(csv$doi,.f = \(x) try(suppressMessages(RefManageR::GetBibEntryWithDOI(x)),silent = TRUE),.progress = TRUE)

  bad <- which(purrr::map_lgl(bibEntry,is.null) |
                 purrr::map_lgl(bibEntry,\(x) length(x) == 0) |
                 purrr::map_lgl(bibEntry,\(x) is(x,"try-error")) )

  gbe <- bibEntry[-bad]

  df <- purrr::map_dfr(gbe,as.data.frame)

  df$citekey <- csv$citekey[-bad]

  return(df)
}


specialFixes <- function(){

  ### add Street Perrot reference to oxford lake status  data
  path = "~/Dropbox/lipdverse/html/lipdverse/bibDsid.json"
  dsidCitekey <- jsonlite::read_json(path)
  cb <- map_chr(D,"createdBy")
  ox <- which(cb == "http://github.com/nickmckay/oxfordLakeStatus2Lipd")
  dsid <- map_chr(D,"datasetId")
  dsidOx <- dsid[ox]

  for(thisId in dsidOx){
    dsidCitekey[[thisId]]$key <- unique(c(dsidCitekey[[thisId]]$key,"streetperrott1989oxfordlakestatus"))
  }

  j <- jsonlite::toJSON(dsidCitekey, pretty=TRUE, auto_unbox = TRUE)
  write(j, file=path)
  system("scp /Users/nicholas/Dropbox/lipdverse/html/lipdverse/bibDsid.json npm4@linux.cefns.nau.edu:/www/cefns.nau.edu/seses/lipdverse/lipdverse/bibDsid.json")



}
