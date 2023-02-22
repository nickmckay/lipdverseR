#load in past json

getPastDataframe <- function(filename = "past.json"){
  PaST <- jsonlite::read_json(filename)
  PaST <- PaST$`@graph`

  allIdUrls <- purrr::map_chr(PaST,"@id")
  ids <- stringr::str_extract(allIdUrls,"\\d{1,}")

  definitions <- purrr::map_chr(PaST,\(x){y <- x$`http://www.w3.org/2004/02/skos/core#definition`$`@value`; ifelse(is.null(y),NA,y)})

  prefLabel <- purrr::map_chr(PaST,\(x){y <- x$`http://www.w3.org/2004/02/skos/core#prefLabel`$`@value`; ifelse(is.null(y),NA,y)})


  past <- data.frame(id = ids, url = allIdUrls,name = prefLabel, definition = definitions)

  return(past)
}



getLipdVocab <- function(key,
                         PaST =  getPastDataframe()){

  #LiPD variable info from QC sheet.

  lipdInfo <- googlesheets4::read_sheet(ss = key$`googlesheets id`[1],col_types = "c")

  #if variableName
  if(key$name == "paleoData_variableName"){
    lipdInfo <- filter(lipdInfo,is.na(paleoData_isAssemblage))
  }
  uLipdName <- unique(lipdInfo$lipdName)

  dict <- vector(mode = "list",length = length(uLipdName))
  names(dict) <- uLipdName
  for(i in 1:length(dict)){
    this <- list()
    this$name <- uLipdName[i]

    #get pastInfo
    li <- dplyr::filter(lipdInfo,lipdName == uLipdName[i])

    this$pastId <- unique(li$paleoData_pastId)
    if(length(this$pastId) != 1){
      print(li)
      stop("there should be exactly one pastId associated with this name")
    }


    if(is.na(this$pastId)){
      this$pastName <- "This concept doesn't exist on PaST"
      this$pastUrl <- NA
      this$definition <- na.omit(unique(li$definition))
      if(length(this$definition) == 0){#we haven't written one yet.
        this$definition <- "TBD"
        this$definitionSource <- "TBD"
      }else if(length(this$definition) == 1){
        this$definitionSource <- "LinkedEarth"
      }else{
        stop(glue("multiple definitions for {this$name}"))
      }
    }else if(this$pastId == "missing"){
      this$pastName <- "This concept doesn't exist on PaST, yet"
      this$pastUrl <- NA
      this$definition <- na.omit(unique(li$definition))
      if(length(this$definition) == 0){#we haven't written one yet.
        this$definition <- "TBD"
        this$definitionSource <- "TBD"
      }else if(length(this$definition) == 1){
        this$definitionSource <- "LinkedEarth"
      }else{
        stop(glue("multiple definitions for {this$name}"))
      }
    }else{
      pi <- which(PaST$id == this$pastId)
      if(length(pi) != 1){
        print(this)
        stop("bad")
      }
      this$pastName <- PaST$name[pi]
      this$definition <- PaST$definition[pi]
      this$definitionSource <- "PaST"
      this$pastUrl <- PaST$url[pi]
    }




    this$synonym <- unique(li$synonym)
    this$synonym <- setdiff(this$synonym,this$name)

    dict[[i]] <- this
  }
  return(dict)
}

library(tidyverse)
library(glue)
writeVocabMarkdown <- function(voc){
  anchor <- str_replace_all(voc$name,"/","_") %>%
    str_replace_all(" ","_") %>%
    str_replace_all("\\{","_") %>%
    str_replace_all("\\}","_")

  thisTerm <- paste0("### [",voc$name,"](#",anchor,"){#",anchor,"}\n") %>%
    str_c("\n\n") %>%
    str_c("#### Definition\n") %>%
    str_c(glue("{voc$definition}\n\n")) %>%
    str_c("\n\n") %>%
    str_c(glue("*source: {voc$definitionSource}*\n\n")) %>%
    str_c("\n\n") %>%
    str_c("#### Known synonyms\n") %>%
    str_c(glue("{paste(voc$synonym,collapse = ', ')}\n\n")) %>%
    str_c("\n\n")

  if(!is.na(voc$pastUrl)){
    thisTerm <- thisTerm %>%
      str_c("#### NOAA PaST Thesaurus term\n") %>%
      str_c(glue('[<font size="4">{voc$pastName}</font>]({voc$pastUrl}) ({voc$pastUrl})\n\n')) %>%
      str_c("\n\n")
  }else{
    thisTerm <- thisTerm %>%
      str_c("#### NOAA PaST Thesaurus term\n") %>%
      str_c(glue('<font size="4">{voc$pastName}</font>\n\n')) %>%
      str_c("\n\n")
  }

  #finish
  thisTerm <- thisTerm %>%
    str_c("---") %>%
    str_c("\n\n")

  return(thisTerm)
}


updateVocabWebsites <- function(){

  allKeys <- googlesheets4::read_sheet("16edAnvTQiWSQm49BLYn_TaqzHtKO9awzv5C-CemwyTY")

  for(k in 1:nrow(allKeys)){
    key <- allKeys[k,]
    PaST <- getPastDataframe()
    dict <- getLipdVocab(key,PaST = PaST)

    allVar <- map_chr(dict,writeVocabMarkdown)

    #write YAML header
    yaml <- "---\n" %>%
      str_c(glue("title: {key$name}\n\n")) %>%
      str_c(glue("categories: [vocabulary]\n\n")) %>%
      str_c(glue("summary: {key$summary}\n\n")) %>%
      str_c(glue("---\n\n"))

    #write markdown intro
    intro <- glue("# {key$name}\n") %>%
      str_c("\n\n") %>%
      glue("{key$summary}\n\n") %>%
      str_c("\n\n") %>%
      str_c("---") %>%
      str_c("\n\n")

    page <- paste(c(yaml, intro, allVar),collapse = "\n\n")

    lipdversedir <- file.path("~/Github/lipdverse-website/content/vocabulary/",tolower(key$name))
    if(!dir.exists(lipdversedir)){
      dir.create(lipdversedir)
    }

    page %>% write_file(file = file.path(lipdversedir,"index.Rmd"))

  }
}

writeValidationReportToQCSheet <- function(validationReport,qcId){
  for(i in 1:length(validationReport)){
    tv <- validationReport[[i]]
    if(is(tv,"data.frame")){
      sheetName <- paste0(names(tv)[ncol(tv)],"-invalid")
      tvo <- dplyr::select(tv, -rowNum)
      if(nrow(tvo) == 0){#delete it
        try(googlesheets4::sheet_delete(ss = qcId,sheet = sheetName),silent = TRUE)
      }else{
        cnames <- names(tvo)
        cnames[ncol(tvo)] <- paste0("invalid_",cnames[ncol(tvo)])
        names(tvo) <- cnames
        write_sheet_retry(tvo,ss = qcId, sheet = sheetName)
      }

    }else{#for nested data frames
      for(j in 1:length(tv)){
        sheetName <-  paste0(names(tv[[j]])[5],"-invalid")
        tvo <-  dplyr::select(tv[[j]],-rowNum)
        if(ncol(tvo) == 0){#delete it
          try(googlesheets4::sheet_delete(ss = qcId,sheet = sheetName),silent = TRUE)
        }else{
          cnames <- names(tvo)
          cnames[ncol(tvo)] <- paste0("invalid_",cnames[ncol(tvo)])
          names(tvo) <- cnames
          write_sheet_retry(tvo,ss = qcId, sheet = sheetName)
        }

      }
    }
  }
}
