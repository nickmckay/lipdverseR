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
      }else{
        cnames <- names(tvo)
        cnames[ncol(tvo)] <- paste0("invalid_",cnames[ncol(tvo)])
        names(tvo) <- cnames
        write_sheet_retry(tvo,ss = qcId, sheet = sheetName)
      }

    }else{#for nested data frames
      toDelete <- TRUE
      for(j in 1:length(tv)){
        sheetName <-  paste0(names(tv[[j]])[5],"-invalid") %>% str_remove_all("[0-9]")
        tvo <-  dplyr::select(tv[[j]],-rowNum)
        if(nrow(tvo) > 0){#don't delete it
          cnames <- names(tvo)
          cnames[4] <- str_remove_all(pattern = "[0-9]",paste0("invalid_",cnames[4]))
          names(tvo) <- cnames
          tvo$number <- j

          if(toDelete){#if it's the first good one
            tvbig <- tvo
          }else{
            tvbig <- bind_rows(tvbig,tvo)
          }
          toDelete <- FALSE

        }

      }
      if(toDelete){
        try(googlesheets4::sheet_delete(ss = qcId,sheet = sheetName),silent = TRUE)
      }else{
        write_sheet_retry(tvbig,ss = qcId, sheet = sheetName)
      }

    }
  }
}

resolveQcConflict <- function(qc){
  #which cells have conflicts
  conf <- purrr::map(qc,\(qc) which(grepl(x = qc,"(((",fixed = TRUE) & grepl(x = qc,")))",fixed = TRUE)))
count <- 0
total <- length(unlist(conf))
if(total == 0){
  return(qc)
}

  for(i in 1:length(conf)){
    thisConf <- conf[[i]]
    if(length(conf) > 0){
      for(co in thisConf){
        count = count +1
        cat(crayon::red(glue::glue("Conflict {count} of {total}\n\n")))

        cat(crayon::bold(names(qc)[i]))
        cat("\n\n")

        #parse the three parts
        ts <- qc[[i]][co]
        firstTripleOpen <- str_locate(ts,coll("((("))
        firstTripleClose <- str_locate(ts,coll(")))"))
        lastTripleSlash <- str_locate_all(ts,coll("///"))
        lastTripleSlash <- lastTripleSlash[[length(lastTripleSlash)]]
        lastTripleSlash <- lastTripleSlash[nrow(lastTripleSlash),]


        first <- stringr::str_sub(ts,start = firstTripleOpen[2] + 2,firstTripleClose[1] - 2)
        mid <- stringr::str_sub(ts,firstTripleClose[2] + 2,end = lastTripleSlash[1] - 2)
        last <- stringr::str_sub(ts,lastTripleSlash[2] + 2)


        print(glue("1. Old version: {first}\n\n"))
        print(glue("2. QC Sheet version: {mid}\n\n"))
        print(glue("3. LiPD file version: {last}\n\n"))
        print(glue("4. Don't make any changes\n\n"))
        print(glue("Or type in your preference\n\n"))
        cat("\n\n")
        wg <- askUser("Which option do you prefer?")

        if(wg == 1){
          qc[[i]][co] <- first
        }else if(wg == 2){
          qc[[i]][co] <- mid
        }else if(wg == 3){
          qc[[i]][co] <- last
        }else if(wg == 4){
          print("skipping...")
        }else{
          qc[[i]][co] <- wg
        }

      }
    }
  }
return(qc)

}


