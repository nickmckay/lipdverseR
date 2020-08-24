tibDiff <- function(tcol){
  #check for a change
  old <- tcol[1]
  new <- tcol[2]
  if(is.na(old) | tolower(old) == "na" | is.null(old)){
    old <- ""
  }
  if(is.na(new) | tolower(new) == "na" | is.null(new)){
    new <- ""
  }

  if(old != new){
    return(glue::glue("'{old}' has been replaced by '{new}'"))
  }
}

createChangelog <- function(Lold,
                            Lnew,
                            good.base = c("archiveType",
                                          "createdBy",
                                          "dataSetName",
                                          "dataSource",
                                          "notes",
                                          "originalDataUrl"),
                            exclude.paleo = c("paleoData_meanValue12k",
                                              "paleoData_medianRes12k",
                                              "paleoData_tableName",
                                              "paleoData_values"),
                            exclude.chron = c("chronData_values")){

  cl <- c() #initialize changelog
  ct <- c() #initialize change type
  cv <- c() #initialize change variable
  checked.base <- FALSE #initialize

  # Check for paleo and chronData -------------------------------------------
  #paleoData
  npdo <- length(Lold$paleoData)
  npdn <- length(Lnew$paleoData)

  if(npdn>0){
    hasPaleo <- TRUE
  }else{
    hasPaleo <- FALSE
  }
  #if paleoData was added or removed, let's report that
  if(npdo == 0 & npdn > 0){
    cl <- c(cl,"PaleoData has been added to this dataset")
    ct <- c(ct,"Dataset")
    cv <- c(cv,NA)
  }
  if(npdo > 0 & npdn == 0){
    cl <- c(cl,"All PaleoData have been removed from this dataset")
    ct <- c(ct,"Dataset")
    cv <- c(cv,NA)
  }

  #chronData
  ncdo <- length(Lold$chronData)
  ncdn <- length(Lnew$chronData)

  if(ncdn>0){
    hasChron <- TRUE
  }else{
    hasChron <- FALSE
  }
  #if chronData was added or removed, let's report that
  if(ncdo == 0 & ncdn > 0){
    cl <- c(cl,"chronData has been added to this dataset")
    ct <- c(ct,"Dataset")
    cv <- c(cv,NA)

  }
  if(ncdo > 0 & ncdn == 0){
    cl <- c(cl,"All chronData have been removed from this dataset")
    ct <- c(ct,"Dataset")
    cv <- c(cv,NA)

  }


  # Go through paleoData ----------------------------------------------------
  if(hasPaleo){
    #get tibbles
    to <- Lold %>%
      extractTs() %>%
      ts2tibble() %>%
      dplyr::arrange(paleoData_TSid)

    tn <- Lnew %>%
      extractTs() %>%
      ts2tibble() %>%
      dplyr::arrange(paleoData_TSid)

    #check TSids are unique
    if(any(duplicated(to$paleoData_TSid))){
      stop("the original dataset has duplicated TSids")
    }
    if(any(duplicated(tn$paleoData_TSid))){
      stop("the new dataset has duplicated TSids")
    }


    # Check for added/removed columns -----------------------------------------

    #check for added columns
    if(any(!tn$paleoData_TSid %in% to$paleoData_TSid)){#then one was added
      wa <- which(!tn$paleoData_TSid %in% to$paleoData_TSid)
      for(i in wa){
        cl <- c(cl,
                glue("Column '{tn$paleoData_TSid[i]}', with variable name '{tn$paleoData_variableName[i]}', was added to the dataset")
        )
        ct <- c(ct,"PaleoData table")
        cv <- c(cv,NA)
      }
      #then remove them - we won't describe the details of added columns
      tn <- tn[-wa,]
    }

    #check for removed columns
    if(any(!to$paleoData_TSid %in% tn$paleoData_TSid)){#then one was added
      wr <- which(!to$paleoData_TSid %in% tn$paleoData_TSid)
      for(i in wr){
        cl <- c(cl,
                glue("Column '{to$paleoData_TSid[i]}', with variable name '{to$paleoData_variableName[i]}', was removed from the dataset")
        )
        ct <- c(ct,"PaleoData table")
        cv <- c(cv,NA)

      }
      #then remove them - this should force the datasets to always have the same number of columns
      to <- to[-wr,]
    }

    #make sure there are some rows remaining
    if(nrow(tn) < 2 | nrow(to) < 2){
      stop("there are 0 or 1 matching TSids in the paleoData. You probably entered an incorrect file")
    }

    #check to make sure that the TSids and number of rows are identical
    tn <- tn %>% dplyr::arrange(paleoData_TSid)
    to <- to %>% dplyr::arrange(paleoData_TSid)


    # Check base metadata -----------------------------------------------------

    #make sure the names are present
    ne <- good.base[!good.base %in% names(to)]

    if(length(ne) > 0){
      for(n in ne){
        to <- dplyr::mutate(to,!!n := NA)
      }
    }

    ne <- good.base[!good.base %in% names(tn)]

    if(length(ne) > 0){
      for(n in ne){
        tn <- dplyr::mutate(tn,!!n := NA)
      }
    }

    #filter and collapse to just non-paleo metadata




    bto <- dplyr::select(to,!!good.base,
                         starts_with("pub"),
                         starts_with("geo_"),
                         starts_with("funding")) %>%
      dplyr::distinct() %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))


    #should only be one row now
    if(nrow(bto) != 1){
      stop("The old dataset has discrepencies in metadata between columns that shouldn't exist")
    }


    btn <- dplyr::select(tn,!!good.base,
                         starts_with("pub"),
                         starts_with("geo_"),
                         starts_with("funding")) %>%
      dplyr::distinct() %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))


    if(nrow(btn) != 1){
      stop("The new dataset has discrepencies in metadata between columns that shouldn't exist")
    }

    tcdf <- dplyr::bind_rows(bto,btn)
    tcdf[is.null(tcdf)] <- NA

    #check for changes and report back
    cldf <- purrr::map_dfc(tcdf,tibDiff)
    if(nrow(cldf)>0){
      #fold into changelog
      cl <- c(cl,paste(names(cldf),cldf,sep = ": "))
      #what type of base change?
      for(cli in 1:ncol(cldf)){
        if(grepl("pub[0-9]_",names(cldf)[cli])){
          ct <- c(ct,"Publication metadata")
        }else if(grepl("funding[0-9]_",names(cldf)[cli])){
          ct <- c(ct,"Funding metadata")
        }else if(grepl("geo_",names(cldf)[cli])){
          ct <- c(ct,"Geographic metadata")
        }else{
          ct <- c(ct,"Base metadata")
        }
      }
      cv <- c(cv,names(cldf))
    }

    checked.base <- TRUE
    # Check paleoData column metadata -----------------------------------------

    paleoSelect <- function(x,exclude.paleo){

      exclude.paleo <- exclude.paleo[exclude.paleo %in% names(x)]

      o <- dplyr::select(x,starts_with("paleoData_"),
                         starts_with("interpretation"),
                         starts_with("calibration")) %>%
        dplyr::select(-starts_with("paleoData_has"),
                      -!!exclude.paleo)
      return(o)}


    #make all columns character for this comparison
    ptn <- paleoSelect(tn,exclude.paleo) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))
    pto <- paleoSelect(to,exclude.paleo) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))

    #loop through TSids
    for(i in 1:nrow(ptn)){
      tsi <- ptn$paleoData_TSid[i]
      tsname <- ptn$paleoData_variableName[i]

      #prep the comparison
      tcdf <- dplyr::bind_rows(pto[i,],ptn[i,])
      tcdf[is.null(tcdf)] <- NA

      #check for changes and report back
      cldf <- purrr::map_dfc(tcdf,tibDiff)
      #fold into changelog

      if(nrow(cldf) > 0){
        cl <- c(cl,
                paste(glue::glue("{tsname} ({tsi})"),names(cldf),cldf,sep = ": "))
        for(cli in 1:ncol(cldf)){
          if(grepl(pattern = "interpretation[0-9]_",names(cldf)[cli])){
            ct <- c(ct,"Paleo Interpretation metadata")
          }else if(startsWith(prefix = "calibration",names(cldf)[cli])){
            ct <- c(ct,"Paleo Calibration metadata")
          }else{
            ct <- c(ct,"Paleo Column metadata")
          }
        }
        cv <- c(cv,names(cldf))

      }
    }



    # compare the paleoData_values --------------------------------------------
    for(i in 1:nrow(tn)){
      tov <- to$paleoData_values[[i]]
      tov[is.na(tov)] <- -999
      tnv <- tn$paleoData_values[[i]]
      tnv[is.na(tnv)] <- -999

      if(!all(tov == tnv)){
        tsi <- tn$paleoData_TSid[i]
        tsname <- tn$paleoData_variableName[i]
        cl <- c(cl,
                glue::glue("{tsname} ({tsi}): The paleoData_values have changed"))
        ct <- c(ct,"PaleoData values")
        cv <- c(cv,"paleoData_values")
      }
    }

  }


  # Go through chronData ----------------------------------------------------
  if(hasChron){
    #get tibbles
    to <- Lold %>%
      extractTs(mode = "chron") %>%
      ts2tibble() %>%
      dplyr::arrange(chronData_TSid)

    tn <- Lnew %>%
      extractTs(mode = "chron") %>%
      ts2tibble() %>%
      dplyr::arrange(chronData_TSid)

    #check TSids are unique
    if(any(duplicated(to$chronData_TSid))){
      stop("the original dataset has duplicated chron TSids")
    }
    if(any(duplicated(tn$chronData_TSid))){
      stop("the new dataset has duplicated chron TSids")
    }


    # Check for added/removed columns -----------------------------------------

    #check for added columns
    if(any(!tn$chronData_TSid %in% to$chronData_TSid)){#then one was added
      wa <- which(!tn$chronData_TSid %in% to$chronData_TSid)
      for(i in wa){
        cl <- c(cl,
                glue("Column '{tn$chronData_TSid[i]}', with variable name '{tn$chronData_variableName[i]}', was added to the dataset")
        )
        ct <- c(ct,"ChronData table")
        cv <- c(cv,NA)

      }
      #then remove them - we won't describe the details of added columns
      tn <- tn[-wa,]
    }

    #check for removed columns
    if(any(!to$chronData_TSid %in% tn$chronData_TSid)){#then one was added
      wr <- which(!to$chronData_TSid %in% tn$chronData_TSid)
      for(i in wr){
        cl <- c(cl,
                glue("Column '{to$chronData_TSid[i]}', with variable name '{to$chronData_variableName[i]}', was removed from the dataset")
        )
        ct <- c(ct,"ChronData table")
        cv <- c(cv,NA)

      }
      #then remove them - this should force the datasets to always have the same number of columns
      to <- to[-wr,]
    }

    #make sure there are some rows remaining
    if(nrow(tn) < 1 | nrow(to) < 1){
      stop("there are 0 or 1 matching TSids in the chronData. You probably entered an incorrect file")
    }

    #check to make sure that the TSids and number of rows are identical
    tn <- tn %>% dplyr::arrange(chronData_TSid)
    to <- to %>% dplyr::arrange(chronData_TSid)


    # Check base metadata -----------------------------------------------------
    if(!checked.base){
      #make sure the names are present
      ne <- good.base[!good.base %in% names(to)]

      if(length(ne) > 0){
        for(n in ne){
          to <- dplyr::mutate(to,!!n := NA)
        }
      }

      ne <- good.base[!good.base %in% names(tn)]

      if(length(ne) > 0){
        for(n in ne){
          tn <- dplyr::mutate(tn,!!n := NA)
        }
      }

      #filter and collapse to just non-chron metadata




      bto <- dplyr::select(to,!!good.base,
                           starts_with("pub"),
                           starts_with("geo_"),
                           starts_with("funding")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))


      #should only be one row now
      if(nrow(bto) != 1){
        stop("The old dataset has discrepencies in metadata between columns that shouldn't exist")
      }


      btn <- dplyr::select(tn,!!good.base,
                           starts_with("pub"),
                           starts_with("geo_"),
                           starts_with("funding")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))


      if(nrow(btn) != 1){
        stop("The new dataset has discrepencies in metadata between columns that shouldn't exist")
      }

      tcdf <- dplyr::bind_rows(bto,btn)
      tcdf[is.null(tcdf)] <- NA

      #check for changes and report back
      cldf <- purrr::map_dfc(tcdf,tibDiff)
      if(nrow(cldf)>0){
        #fold into changelog
        cl <- c(cl,paste(names(cldf),cldf,sep = ": "))
        #what type of base change?
        for(cli in 1:ncol(cldf)){
          if(grepl("pub[0-9]_",names(cldf)[cli])){
            ct <- c(ct,"Publication metadata")
          }else if(grepl("funding[0-9]_",names(cldf)[cli])){
            ct <- c(ct,"Funding metadata")
          }else if(grepl("geo_",names(cldf)[cli])){
            ct <- c(ct,"Geographic metadata")
          }else{
            ct <- c(ct,"Base metadata")
          }
        }
        cv <- c(cv,names(cldf))

      }

      checked.base <- TRUE
    }
    # Check chronData column metadata -----------------------------------------

    chronSelect <- function(x,exclude.chron){
      exclude.chron <- exclude.chron[exclude.chron %in% names(x)]

      o <- dplyr::select(x,starts_with("chronData_"),
                         starts_with("interpretation"),
                         starts_with("calibration")) %>%
        dplyr::select(-starts_with("chronData_has"),
                      -!!exclude.chron)
      return(o)}


    #make all columns character for this comparison
    ptn <- chronSelect(tn,exclude.chron) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))
    pto <- chronSelect(to,exclude.chron) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(),as.character))

    #loop through TSids
    for(i in 1:nrow(ptn)){
      tsi <- ptn$chronData_TSid[i]
      tsname <- ptn$chronData_variableName[i]

      #prep the comparison
      tcdf <- dplyr::bind_rows(pto[i,],ptn[i,])
      tcdf[is.null(tcdf)] <- NA

      #check for changes and report back
      cldf <- purrr::map_dfc(tcdf,tibDiff)
      #fold into changelog

      if(nrow(cldf) > 0){
        cl <- c(cl,
                paste(glue::glue("{tsname} ({tsi})"),names(cldf),cldf,sep = ": "))
        ct <- c(ct,
                rep("Chron Column metadata",times = ncol(cldf)))
        cv <- c(cv,names(cldf))

      }
    }



    # compare the chronData_values --------------------------------------------
    for(i in 1:nrow(tn)){
      #change NAs to allow testing
      tov <- to$chronData_values[[i]]
      tov[is.na(tov)] <- -999
      tnv <- tn$chronData_values[[i]]
      tnv[is.na(tnv)] <- -999
      if(!all(tov == tnv)){
        tsi <- tn$chronData_TSid[i]
        tsname <- tn$chronData_variableName[i]
        cl <- c(cl,
                glue::glue("{tsname} ({tsi}): The chronData_values have changed"))
        ct <- c(ct,"ChronData values")
        cv <- c(cv,"chronData_values")

      }
    }

  }

  if(length(cl)>0){
  changelog <- tibble::tibble(type = ct, change = cl, variable = cv,dataSetName = Lnew$dataSetName)
  }else{
    changelog <- tibble::tibble(type = ct, change = cl, variable = cv,dataSetName = NULL)
  }

  return(changelog)
}


updateChangelog <- function(L,
                            changelog,
                            version = NA,
                            notes = NA,
                            curator = Sys.info()[["user"]],
                            timestamp = lubridate::now(tzone = "UTC")){

  if(nrow(changelog) == 0){#no changes, don't write
    return(L)
  }

  #restrict to just type and change for now
  changelog <- dplyr::select(changelog,type,change)

  #get prior changes
  allChanges <- L$changelog

  #prepare changelog
  changes <- changelog %>% dplyr::group_by(type) %>% tidyr::nest()
  #change to list structure
  changelist <- setNames(changes[[2]],changes[[1]]) %>%
    map(as.matrix) %>%
    map(setNames,NULL)

  getVers <- function(x){as.character(x$version)}

  #version check and increment
  if(is.na(version)){#automatically increment version
    lastVers <- max(as.numeric_version(purrr::map_chr(allChanges,getVers)))
    #check to make sure there is a previous version
    if(length(lastVers)==0){
      stop("there don't seem to be any previous versions - version must be specified")
    }
    #initialize
    vers <- lastVers
    #increment intelligently
    if(any(grepl("values",names(changelist)))){#minor
      vers[[1,2]] <- lastVers[[1,2]]+1
      vers[[1,3]] <- 0
    }else{#patch
      vers[[1,3]] <- lastVers[[1,3]]+1
    }

  }else{
    #increment intelligently
    if(version == "major"){
      if(is.na(notes)){
        stop("the `notes` field must be present for a major version change")
      }
      lastVers <- max(as.numeric_version(purrr::map_chr(allChanges,getVers)))
      #check to make sure there is a previous version
      if(length(lastVers)==0){
        stop("there don't seem to be any previous versions - version must be specified")
      }
      vers[[1,1]] <- lastVers[[1,1]]+1
      vers[[1,2:3]] <- 0
    }else{
      vers <- as.numeric_version(version)
    }
  }


  if(!is.na(notes)){#add in notes if present
    thisChange <- list(version = as.character(vers),
                       curator = curator,
                       timestamp = paste(timestamp,tz(timestamp)),
                       notes = notes,
                       changes =  changelist)
  }else{
    #create this instance of the changelog
    thisChange <- list(version = as.character(vers),
                       curator = curator,
                       timestamp = paste(timestamp,tz(timestamp)),
                       changes =  changelist)
  }

  #update the changes
  L$changelog <- append(list(thisChange),L$changelog)

  return(L)

}

getChangelog <- function(L,version = "newest"){
  allvers <- as.numeric_version(purrr::map_chr(L$changelog,"version"))
  if(length(allvers) == 0){
    stop("This dataset does not appear to have a changelog yet. Use createChangelog() and updateChangelog() to add one.")
  }

  if(grepl(pattern = "new",version,ignore.case = T)){
    wv <- which(allvers == max(allvers))
  }else if(grepl(pattern = "previous",version,ignore.case = T)){
    wv <- which(allvers == sort(allvers,decreasing = T)[2])
  }else if(grepl(pattern = "old",version,ignore.case = T)){
    wv <- which(allvers == min(allvers))
  }else{#try to match the version
    wv <- which(allvers == as.numeric_version(version))
  }

  if(length(wv) == 0){
    stop("failed to find a match for this version")
  }

  if(length(wv) > 1){
    stop("Found multiple matches for this version. This is bad.")
  }

  #spit it out
  return(L$changelog[[wv]])

}

getVersion <- function(L){
  version <- as.numeric_version(purrr::map_chr(L$changelog,"version")) %>%
    max() %>%
    as.character()

  if(length(version)==0){
    version <- "empty"
  }

  return(version)
}


createMarkdownChangelog <- function(L){
  cl <- L$changelog
  mdcl <- glue::glue("# Version history for {L$dataSetName}") %>%
    str_c("\n\n")


  for(i in 1:length(cl)){
    mdcl <- mdcl %>%
      str_c(createSingleMarkdownChangelog(cl[[i]])) %>%
      str_c("\n\n")
  }

  mdcl <- stringr::str_replace_all(mdcl,pattern = "''",replacement = "NULL")
  return(mdcl)
}

createSingleMarkdownChangelog<- function(scl){
  #seed version
  clmd <- glue("### Version: {scl$version} \n") %>%
    str_c("\n")

  lev1 <- names(scl)
  for(l1 in 1:length(lev1)){
    tb <- lev1[l1]
    #do level one changes
    if(is.character(scl[[l1]])){#then write the bullets
      clmd <- str_c(clmd,glue("* *{lev1[l1]}*: {scl[[l1]]}\n")) %>%
        str_c("\n")
    }else{
      clmd <- str_c(clmd,glue("* *{lev1[l1]}*:\n")) %>%
        str_c("\n")

      lev2 <- scl[[l1]]

      for(l2 in 1:length(lev2)){
        thisType <- names(lev2)[l2]
        clmd <- str_c(clmd,glue("\t + *{thisType}*:\n",.trim = FALSE))
        theseChanges <- lev2[[l2]]
        for(t in 1:length(theseChanges)){
          clmd <- str_c(clmd,glue("\t \t - {theseChanges[t]}\n",.trim = FALSE))
        }
        clmd <- str_c(clmd,"\n")
      }
    }
  }

  return(clmd)

}

createDatasetId <- function(){
  return(paste(sample(c(letters,LETTERS,0:9),size = 20,replace = T),collapse = ""))
}


#show the most recent changes of all files that changed
createProjectChangelogLong <- function(Dchanged,proj,projVers){
  mdcl <- glue::glue("# Detailed changelog for {proj} version {projVers}") %>%
    str_c("\n\n")

  for(d in 1:length(Dchanged)){
    tcl <- getChangelog(Dchanged[[d]],version = "newest")
    mdcl <- mdcl %>%
      glue::glue("## {Dchanged[[d]]$dataSetName} version {getVersion(Dchanged[[d]])}") %>%
      str_c("\n\n") %>%
      str_c(createSingleMarkdownChangelog(tcl)) %>%
      str_c("\n\n")
  }

  mdcl <- stringr::str_replace_all(mdcl,pattern = "''",replacement = "NULL")
  return(mdcl)
}

#create initial changelogs and datasetId



#for(d in 1:length(Do)){



#Dn <- D

# for(d in 1:length(Do)){
#   dsn <- Do[[d]]$dataSetName
#   #cl <- createChangelog(Do[[dsn]],D[[dsn]])
#   Dn[[dsn]] <- updateChangelog(D[[dsn]],changelog = tibble(type = "Setup",change = "Changelog created"),notes = "This marks the creation of the changelog, and marks the status of this dataset as of Temp12k version 1.0.1",version = "1.0.0")
#   Dn[[dsn]]$datasetId <- createDatasetId()
# }
#
#
# for(dsn in 1:length(Dn)){
#   if(is.null(Dn[[dsn]]$changelog)){
#   Dn[[dsn]] <- updateChangelog(D[[dsn]],changelog = tibble(type = "Setup",change = "Changelog created"),notes = "This marks the creation of the changelog, and marks the status of this dataset as of mid 2020",version = "1.0.0")
#   Dn[[dsn]]$datasetId <- createDatasetId()
#   }
# }
#
  #Dnn <- readLipd("~/Dropbox/lipdverse/database/")
  Dn2 <- list()
  #
  bigCl <- tibble()
  for(d in 1:length(Do)){
   #url check
    dsn <- Do[[d]]$dataSetName

    print(d)
    if(is.null(Dnn[[dsn]]$originalDataUrl) & !is.null(Do[[dsn]]$originalDataUrl)){
      print("copying url")
      Dnn[[dsn]]$originalDataUrl <- Do[[dsn]]$originalDataUrl
    }

    cl <- createChangelog(Do[[dsn]],Dnn[[dsn]])
    if(nrow(cl) > 0){
      bigCl <- bind_rows(bigCl,cl)
    }

   Dn2[[dsn]] <- updateChangelog(Dnn[[dsn]],changelog = cl)
  }

  allVers <- map_chr(Dn2,getVersion)
  lastVers <- rep("1.0.0",times = length(Do))
  diffVers <- which(allVers != lastVers)

  Dchanged <- Dn2[diffVers]
  test <- createProjectChangelogLong(Dchanged,"Temp12k-test","1.0.2")
  write_file(test,"~/Desktop/detailedChangelog.Rmd")
  rmarkdown::render("~/Desktop/detailedChangelog.Rmd")

summarizeProjectChanges <- function(bigCl){

  type_summ <- bigCl %>%
    group_by(type) %>%
    summarise(dataSetChanges = length(unique(dataSetName)),
              totalChanges = n()) %>%
    arrange(desc(dataSetChanges)) %>%
    rename(`Type of change` = type,`Number of datasets with a change` = dataSetChanges,`Total changes` = totalChanges)

  variable_summ <- bigCl %>%
    group_by(variable) %>%
    summarise(dataSetChanges = length(unique(dataSetName)),
              totalChanges = n()) %>%
    arrange(desc(dataSetChanges)) %>%
    rename(`Changed metadata variable` = variable,`Number of datasets with a change` = dataSetChanges,`Total changes` = totalChanges)

}
