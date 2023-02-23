#create files for new web structure

createProjectOverviewPage <- function(project,
                                      projVersion,
                                      webdir = "/Volumes/data/Dropbox/lipdverse/html"){

  vers_ <- str_replace_all(projVersion,"[.]","_")

  projPath <- file.path(webdir,project,projVersion)

  projTitle <- paste(project,"-",projVersion)

  sidebarUrl <- "projectSidebar.html"
  mapUrl <- "map.html"

  #load in template iframe
  template <- readr::read_file(file.path(webdir,"iframeTemplateProject.html"))

  #replace terms and links
  template <- template %>%
    str_replace("ProjectNameHere",projTitle) %>%
    str_replace("sideBarUrlHere",sidebarUrl) %>%
    str_replace("mapUrlHere",mapUrl)

  readr::write_file(template,file = file.path(projPath,"index.html"))


}


#create project sidebar meta .html
createProjectSidebarHtml <- function(project, vers, webdir = "/Volumes/data/Dropbox/lipdverse/html"){


  vers_ <- str_replace_all(vers,"[.]","_")

  #create some strings we'll need

  sidebarTitle <- glue::glue("{project} - {vers}")
  filePath <- paste0(project,vers)
  zipPath <- paste0(filePath,".zip")
  rPath <- paste0(filePath,".RData")
  matlabPath <- paste0(filePath,".mat")
  pythonPath <- paste0(filePath,".pkl")

  #add copy/paste code block

  #prep and
  sidebar <- '<head>\n
<base target="_PARENT">\n
</head>\n'


  #html sidebar
  sidebar <- sidebar %>%
    str_c('<div class="sidenav"> \n') %>%
    str_c(glue('<h2>{sidebarTitle}</h2>',sep = "\n")) %>%
    str_c(glue('<p style="margin-left: 0px"><a href="{zipPath}">Download all LiPD files</a>',sep = "\n")) %>%
    str_c("\n") %>%
    str_c("            \n") %>%
    str_c(glue('<p style="margin-left: 0px"><a href="{rPath}">Download R serialization</a>',sep = "\n")) %>%
    str_c("\n") %>%
    str_c("            \n") %>%
    str_c(glue('<p style="margin-left: 0px"><a href="{matlabPath}">Download Matlab serialization</a>',sep = "\n")) %>%
    str_c("\n") %>%
    str_c("            \n") %>%
    str_c(glue('<p style="margin-left: 0px"><a href="{pythonPath}">Download python (pickle) serialization</a>',sep = "\n")) %>%
    str_c("\n") %>%
    str_c("            \n") %>%
    str_c(glue('<p style="margin-left: 0px"><a href="https://github.com/nickmckay/LiPDverse/issues">Report an issue (include project name)</a>',sep = "\n")) %>%
    str_c("\n") %>%
    str_c("            \n")


  if(project != "lipdverse"){
    sidebar <- sidebar %>%
      str_c(glue('<p style="margin-left: 0px"><a href="changelogSummary.html">View summary changelog</a>',sep = "\n")) %>%
      str_c("\n") %>%
      str_c("            \n") %>%
      str_c(glue('<p style="margin-left: 0px"><a href="changelogDetail.html">View detailed changelog</a>',sep = "\n")) %>%
      str_c("\n") %>%
      str_c("            \n")
  }

  ######END METADATA!!!!############################
  sidebar <- sidebar %>%
    str_c("</div>")


  if(!dir.exists(file.path(webdir,project,vers_))){
    dir.create(file.path(webdir,project,vers_),recursive = TRUE)
  }
  write_file(sidebar,file = file.path(webdir,project,vers_,"projectSidebar.html"))



}

#create sidebarmeta .html
createSidebarHtml <- function(L,webdir = "/Volumes/data/Dropbox/lipdverse/html"){

  ts <- extractTs(L)

  #check to see if it has an ensemble
  Lne <- removeEnsembles(L)

  if(object.size(L) > object.size(Lne)){#there's an ensemble
    hasEnsembles <- TRUE
  }else{
    hasEnsembles <- FALSE
  }

  #check for these variables

  dsid <- L$datasetId
  dsn <- L$dataSetName
  vers <- sapply(L$changelog,"[[","version") %>% as.numeric_version() %>% max() %>% as.character()
  vers_ <- str_replace_all(vers,"[.]","_")
  mric <- getMostRecentInCompilationsTs(ts) %>%
    na.omit() %>%
    paste(collapse = ", ") %>%
    str_split(", ") %>%
    pluck(1) %>%
    unique()


  #create some strings we'll need

  sidebarTitle <- glue::glue("{dsn} - v{vers}")
  dsidstr <- paste("Dataset Id:",dsid)
  dsPath <- glue("../../{dsid}/{vers_}/")
  lpdPath <- glue("{dsPath}/{dsn}.lpd")
  lpdPathEns <- glue("{dsPath}/{dsn}-ensemble.lpd")
  jsonPath <- glue("{dsPath}/{dsn}.jsonld")
  csvName <- glue("{dsPath}/{dsn}.csv")
  csvNameChron <- glue("{dsPath}/{dsn}-chron.csv")
  lipdverseLpdPath <- glue("https://lipdverse.org/data/{dsid}/{vers_}/{dsn}.lpd")

  #add copy/paste code block


  #prep and setup widget
  sidebar <- '<head>\n
<base target="_PARENT">\n
<link rel="stylesheet" href="../../../sidebarstyles.css">
</head>\n'




  #html sidebar

  if(hasEnsembles){

    sidebar <- sidebar %>%
      str_c('<div class="sidenav"> \n') %>%
      str_c(glue('<h2>{sidebarTitle}</h2>',sep = "\n")) %>%
      str_c(glue('<h4><a href="{dsPath}">{dsidstr}</a></h4>',sep = "\n")) %>%
      str_c(glue('<p style="margin-left: 0px"><a href="{lpdPath}">Download LiPD file without ensemble tables</a>',sep = "\n")) %>%
      str_c("\n") %>%
      str_c("            \n") %>%
      str_c(glue('<p style="margin-left: 0px"><a href="{jsonPath}">Download LiPD file as JSON-LD (no ensemble data)</a>',sep = "\n")) %>%
      str_c("\n") %>%
      str_c("            \n") %>%
      str_c(glue('<p style="margin-left: 0px"><a href="{lpdPathEns}">Download LiPD file with ensemble data</a>',sep = "\n")) %>%
      str_c("\n") %>%
      str_c("            \n")
  }else{
    sidebar <- sidebar %>%
      str_c('<div class="sidenav"> \n') %>%
      str_c(glue('<h2>{sidebarTitle}</h2>',sep = "\n")) %>%
      str_c(glue('<h4><a href="{dsPath}">{dsidstr}</a></h4>',sep = "\n")) %>%
      str_c(glue('<p style="margin-left: 0px"><a href="{lpdPath}">Download LiPD file</a>',sep = "\n")) %>%
      str_c("\n") %>%
      str_c("            \n") %>%
      str_c(glue('<p style="margin-left: 0px"><a href="{jsonPath}">Download LiPD file as JSON-LD</a>',sep = "\n")) %>%
      str_c("\n") %>%
      str_c("            \n")
  }

  #add load this file into R codeblock
  sidebar <- sidebar %>%
    str_c(glue('<h4>R code to load dataset:</h4>')) %>%
    str_c(glue('<pre><code>L <- lipdR::readLipd("{lipdverseLpdPath}")</code></pre>')) %>%
    str_c("\n") %>%
    str_c("            \n") %>%
  str_c(glue('<p style="margin-left: 0px"><a href="http://lipd.net/playground?source={lipdverseLpdPath}">Edit LiPD file</a>',sep = "\n")) %>%
    str_c("\n") %>%
    str_c("            \n") %>%
    str_c(glue('<p style="margin-left: 0px"><a href="{csvName}">Download PaleoData only (csv)</a>',sep = "\n")) %>%
    str_c("\n") %>%
    str_c("            \n")


  if(length(L$chronData) > 0){
    if(length(L$chronData[[1]]$measurementTable) > 0){

      sidebar <- sidebar %>%
        str_c(glue('<p style="margin-left: 0px"><a href="{csvNameChron}">Download ChronData only (csv)</a>',sep = "\n")) %>%
        str_c("\n") %>%
        str_c("            \n")
    }
  }

  sidebar <- sidebar %>%
    str_c(glue('<p style="margin-left: 0px"><a href="changelog.html">Dataset changelog</a>',sep = "\n")) %>%
    str_c("\n") %>%
    str_c("            \n") %>%
    str_c('<p style="margin-left: 0px"><a href="https://github.com/nickmckay/LiPDverse/issues">Report an issue (include datasetId and version)</a>',sep = "\n") %>%
    str_c("\n") %>%
    str_c('<p style="margin-left: 0px"><strong>In compilations:</strong> (only most recent versions are shown)</p>\n') %>%
    str_c("\n")


  if(length(mric) > 0){
    for(ic in 1:length(mric)){
      sidebar <-  sidebar %>%
        str_c(glue('<p style="margin-left: 0px"><a href="https://lipdverse.org/{stringr::str_replace(mric[ic],"-","/")}">{mric[ic]}</a>',sep = "\n")) %>%
        str_c("\n")
    }
  }else{
    sidebar <-  sidebar %>%
      str_c(glue('<p style="margin-left: 0px">none</p>',sep = "\n")) %>%
      str_c("\n")
  }




  #get all the variable names
  allNames <- names(ts[[1]])


  ###########Write root metadata
  sidebar <- writeCollapsibleChunks(sidebar,ts,name = "root",vars = c("archiveType", "originalDataUrl","lipdVersion","dataContributor"),open = TRUE)
  ###########End root metadata

  ############WRITE PUB###############
  #large pub section
  pubNames <- allNames[grep("pub*", allNames)]
  if(length(pubNames)>1){
    npub <- max(as.numeric(str_extract_all(pubNames,pattern = "[0-9]")))
  }else{
    npub <- 0
  }

  if(npub>0){
    bigName <-  "pub"
    vars <-  c("author","citeKey","journal","volume","pages","pubYear","title","DOI","doi")

    sidebar <- str_c(sidebar,"<details>",sep = "\n") %>%
      str_c(str_c("<summary>",bigName,"</summary>"),sep = "\n")


    #write each publication.
    for(p in 1:npub){
      sidebar <- writeCollapsibleChunks(sidebar,ts,name = str_c(bigName,as.character(p)),vars = vars,indent = 10)
    }

    #close pub setion
    sidebar <- str_c(sidebar,"</details>",sep = "\n")
  }
  ############END PUB###############

  ##################################Write Funding metadata.................
  pubNames <- allNames[grep("funding*", allNames)]
  if(length(pubNames)>1){
    npub <- max(as.numeric(str_extract_all(pubNames,pattern = "[0-9]")))
  }else{
    npub <- 0
  }

  if(npub>0){
    bigName <-  "funding"
    vars <-  c("agency","grant","country")


    sidebar <- str_c(sidebar,"<details>",sep = "\n") %>%
      str_c(str_c("<summary>",bigName,"</summary>"),sep = "\n")


    #write each publication.
    for(p in 1:npub){
      sidebar <- writeCollapsibleChunks(sidebar,ts,name = str_c(bigName,as.character(p)),vars = vars,indent = 10)
    }

    #close pub setion
    sidebar <- str_c(sidebar,"</details>",sep = "\n")
  }
  ############END FUNDING###############

  ###########Write geo metadata
  sidebar <- writeCollapsibleChunks(sidebar,ts,name = "geo",vars = c("latitude", "longitude","elevation","region","siteName","pages2kRegion","country","state"),open = TRUE)
  ###########End root metadata

  #PaleoData preamble
  sidebar <- str_c(sidebar,"<details open>",sep = "\n") %>%
    str_c(str_c("<summary>PaleoData columns</summary>"),sep = "\n")

  ##################################Write PaleoData Column metadata.................
  ##loop through columns and plot them.
  hasInterpretation <- sapply(ts,function(x){!is.null(x$interpretation1_variable)})
  paleoNum <-  pullTsVariable(ts,"paleoNumber")
  tableNum <-  pullTsVariable(ts,"tableNumber")



  thisVarNames <- sapply(ts,"[[","paleoData_variableName")
  xcol <- which(startsWith(thisVarNames,"age") | startsWith(thisVarNames,"depth") | startsWith(thisVarNames,"year"))

  plotOrder <-  seq(1, length(ts)) #generate a sequence to start with
  isxcol <- plotOrder %in% xcol
  plotOrder <- plotOrder[order(paleoNum,tableNum,-isxcol,-hasInterpretation)]#sort by table Number

  for(cc in plotOrder){#for each column..
    #regular metadata
    if(length(ts[[cc]]$paleoData_units) == 0){
      ts[[cc]]$paleoData_units <- "missing"
    }


    if(is.na(ts[[cc]]$paleoData_units)){
      ts[[cc]]$paleoData_units <- "unitless"
    }

    if(max(paleoNum) == 1  & max(tableNum) == 1){
      tvname <- str_c(ts[[cc]]$paleoData_variableName," (",ts[[cc]]$paleoData_units,")")
    }else{
      tvname <- str_c(ts[[cc]]$paleoData_variableName," (",ts[[cc]]$paleoData_units,") [",as.character(ts[[cc]]$paleoNumber),"-",as.character(ts[[cc]]$tableNumber),"]")
    }

    sidebar <- writeCollapsibleChunks(sidebar,ts,name = "paleoData",forceName = tvname,vars = c("TSid", "variableName","units","description","useInGlobalTemperatureAnalysis"),tsi = cc,dontClose = TRUE,indent = 10)




    ###INTERPRETATIONS
    allNames=names(ts[[cc]])
    pubNames <- allNames[grep("interpretation*", allNames)]

    if(length(pubNames)>1){
      npub <- max(as.numeric(str_extract_all(pubNames,pattern = "[0-9]")))
    }else{
      npub <- 0
    }

    if(npub>0){
      bigName <-  "interpretation"

      sidebar <- str_c(sidebar,"<details>",sep = "\n") %>%
        str_c(str_c("<summary>",bigName,"</summary>"),sep = "\n")


      #write each interpretation
      for(p in 1:npub){
        vars <- str_extract(pubNames,str_c("(?<=interpretation",as.numeric(p),"_).*$")) #get all.
        sidebar <- writeCollapsibleChunks(sidebar,ts,name = str_c(bigName,as.character(p)),vars = vars,tsi = cc,forceName = as.character(p),indent = 20)
      }
      if(is.na(sidebar)){stop("asdas")}

      #close paleo setion
      sidebar <- str_c(sidebar,"</details>",sep = "\n")
    }
    sidebar <- str_c(sidebar,"</details>",sep = "\n")
    ##END INTERPRETATIONS
  }




  ############END PaleoData Column###############
  if(length(L$chronData) > 0){
    if(length(L$chronData[[1]]$measurementTable) > 0){

      chronTS <- extractTs(L,mode = "chron",whichtables = "meas")

      #ChronData preamble
      sidebar <- str_c(sidebar,"<details>",sep = "\n") %>%
        str_c(str_c("<summary>ChronData columns</summary>"),sep = "\n")
      ##################################Write ChronData Column metadata.................
      ##loop through columns and plot them.
      chronNum <-  pullTsVariable(chronTS,"chronNumber")
      tableNum <-  pullTsVariable(chronTS,"tableNumber")



      thisVarNames <- sapply(chronTS,"[[","chronData_variableName")
      xcol <- which(startsWith(thisVarNames,"depth"))


      plotOrderChron <-  seq(1, length(chronTS)) #generate a sequence to start with
      isxcol <- plotOrderChron %in% xcol
      plotOrderChron <- plotOrderChron[order(chronNum,tableNum,-isxcol)]#sort by table Number

      for(cc in plotOrderChron){#for each column..
        #regular metadata

        if(max(chronNum) == 1  & max(tableNum) == 1){
          tvname <- str_c(chronTS[[cc]]$chronData_variableName," (",chronTS[[cc]]$chronData_units,")")
        }else{
          tvname <- str_c(chronTS[[cc]]$chronData_variableName," (",chronTS[[cc]]$chronData_units,") [",as.character(chronTS[[cc]]$chronNumber),"-",as.character(chronTS[[cc]]$tableNumber),"]")
        }

        sidebar <- writeCollapsibleChunks(sidebar,thisTS = chronTS,name = "chronData",forceName = tvname,vars = c("TSid", "variableName","units","description"),tsi = cc,dontClose = TRUE,indent = 10)
        #close each column setion
        sidebar <- str_c(sidebar,"</details>",sep = "\n")
      }

      #######END CHRONDATA!!!!######################
    }
  }

  ######END METADATA!!!!############################
  sidebar <- sidebar %>%
    str_c("</div>")


  if(!dir.exists(file.path(webdir,"data",dsid,vers_))){
    dir.create(file.path(webdir,"data",dsid,vers_),recursive = TRUE)
  }
  write_file(sidebar,file = file.path(webdir,"data",dsid,vers_,"sidebar.html"))



}

# create project map .html

createProjectMapHtml <- function(TS,project,projVersion,webdir = "/Volumes/data/Dropbox/lipdverse/html"){

  projDir <- file.path(webdir,project,projVersion)
  if(!dir.exists(projDir)){
    dir.create(projDir,recursive = TRUE)
  }
  #create map meta

  #All datasets
  dsn <- lipdR::pullTsVariable(TS,"dataSetName")
  ui <- which(!duplicated(dsn))
  udsn <- dsn[ui]
  lat <- lipdR::pullTsVariable(TS,"geo_latitude")[ui]
  lon <- lipdR::pullTsVariable(TS,"geo_longitude")[ui]
  archiveType <- lipdR::pullTsVariable(TS,"archiveType")[ui]
  link <- paste0(udsn,".html") %>%
    str_replace_all("'","_")

  #Organize metadata for map
  map.meta <- data.frame(dataSetName = udsn, #datasetname
                         lat = lat,#lat
                         lon = lon,#lon
                         archiveType = factor(archiveType),#archiveType
                         link = link)#Link

  #save for Rmd
  save(map.meta,file = file.path(projDir,"mapmeta.Rdata"))


  #create RMd
  file.copy(file.path(webdir,"mapOnly.Rmd"),to = file.path(projDir,"map.Rmd"),overwrite = TRUE)

  rmarkdown::render(file.path(projDir,"map.Rmd"))

  maphtml <- readr::read_file(file.path(projDir,"map.html"))
  #find head
  maphtml <- stringr::str_replace(maphtml,"<head>\n",'<head>\n<base target="_parent">\n')

  readr::write_file(maphtml,file.path(projDir,"map.html"))


}


createLipdverseMapHtml <- function(TS,webdir = "/Volumes/data/Dropbox/lipdverse/html"){

  #All datasets
  dsn <- lipdR::pullTsVariable(TS,"dataSetName")
  dsid <- lipdR::pullTsVariable(TS,"datasetId")
  ui <- which(!duplicated(dsid))
  udsn <- dsn[ui]
  udsid <- dsid[ui]
  lat <- lipdR::pullTsVariable(TS,"geo_latitude")[ui]
  lon <- lipdR::pullTsVariable(TS,"geo_longitude")[ui]
  archiveType <- lipdR::pullTsVariable(TS,"archiveType")[ui]
  link <- glue::glue("../data/{udsid}/")

  #Organize metadata for map
  map.meta <- data.frame(dataSetName = udsn, #datasetname
                         lat = lat,#lat
                         lon = lon,#lon
                         archiveType = factor(archiveType),#archiveType
                         link = link)#Link

  #save for Rmd
  save(map.meta,file = file.path(webdir,"data","mapmeta.Rdata"))


  #create RMd
  file.copy(file.path(webdir,"mapOnly.Rmd"),to = file.path(webdir,"data","map.Rmd"),overwrite = TRUE)

  rmarkdown::render(file.path(webdir,"data","map.Rmd"))

  maphtml <- readr::read_file(file.path(webdir,"data","map.html"))
  #find head
  maphtml <- stringr::str_replace(maphtml,"<head>\n",'<head>\n<base target="_parent">\n')

  readr::write_file(maphtml,file.path(webdir,"data","map.html"))

}


#create paleodata plots .html
createPaleoDataPlotHtml <- function(L,webdir = "/Volumes/data/Dropbox/lipdverse/html"){

  ts <- extractTs(L)

  dsid <- L$datasetId
  dsn <- L$dataSetName
  vers <- sapply(L$changelog,"[[","version") %>% as.numeric_version() %>% max() %>% as.character()
  vers_ <- str_replace_all(vers,"[.]","_")


  thisVarNames <- sapply(ts,"[[","paleoData_variableName")

  #graph order
  xcol <- which(startsWith(thisVarNames,"age") | startsWith(thisVarNames,"depth") | startsWith(thisVarNames,"year"))

  hasInterpretation <- sapply(ts,function(x){!is.null(x$interpretation1_variable)})
  paleoNum <-  pullTsVariable(ts,"paleoNumber")
  tableNum <-  pullTsVariable(ts,"tableNumber")

  thisVarNames <- sapply(ts,"[[","paleoData_variableName")

  plotOrder <-  seq(1, length(ts)) #generate a sequence to start with
  isxcol <- plotOrder %in% xcol
  plotOrder <- plotOrder[order(paleoNum,tableNum,-isxcol,-hasInterpretation)]#sort by table Number


  ##create csv output for download
  lengths <- sapply(ts,function(x){length(x$paleoData_values)})+1
  outdf <- data.frame(matrix(NA, nrow = max(lengths), ncol = length(lengths)))

  for(cc in 1:length(plotOrder)){#for each column..
    outdf[1,cc] <- ts[[plotOrder[cc]]]$paleoData_TSid
    outdf[2:lengths[plotOrder[cc]],cc] <- ts[[plotOrder[cc]]]$paleoData_values
    if(max(paleoNum) == 1  & max(tableNum) == 1){
      names(outdf)[cc] <- str_c(ts[[plotOrder[cc]]]$paleoData_variableName," (",ts[[plotOrder[cc]]]$paleoData_units,")")
    }else{
      names(outdf)[cc] <-str_c(ts[[plotOrder[cc]]]$paleoData_variableName," (",ts[[plotOrder[cc]]]$paleoData_units,") [",as.character(ts[[plotOrder[cc]]]$paleoNumber),"-",as.character(ts[[plotOrder[cc]]]$tableNumber),"]")
    }

    #check seasonality
    thisSeason <- as.character(ts[[plotOrder[cc]]]$interpretation1_seasonality)
    if(!is.null(thisSeason)){
      if(length(thisSeason) > 0){
        if(is.character(thisSeason) & !is.na(thisSeason)){
          names(outdf)[cc] <- str_c(names(outdf)[cc]," - ",thisSeason)
        }
      }
    }
    print(names(outdf)[cc])
  }
  outnames <- names(outdf)

  if(any(is.na(outnames))){
    ina <- which(is.na(outnames))
    outnames[ina] <- "Missing name"
  }

  names(outdf) <- outnames

  if(!dir.exists(file.path(webdir,"data",dsid,vers_))){
    dir.create(file.path(webdir,"data",dsid,vers_),recursive = TRUE)
  }

  readr::write_csv(outdf,file.path(webdir,"data",dsid,vers_,paste0(dsn,".csv")))

  if(any(plotOrder %in% xcol)){#then remove those
    graphOrder = plotOrder[-which(plotOrder %in% xcol)]
    graphNames = names(outdf)[-which(plotOrder %in% xcol)]
  }else{
    graphOrder = plotOrder
    graphNames = names(outdf)
  }


  #check for non-numeric columns. Don't plot those.
  nonnumeric <- which(purrr::map_lgl(ts,~ !is.numeric(.x$paleoData_values)))


  if(any(plotOrder %in% xcol)){#then remove those
    graphOrder = plotOrder[-which(plotOrder %in% xcol)]
    graphNames = names(outdf)[-which(plotOrder %in% xcol)]
  }else{
    graphOrder = plotOrder
    graphNames = names(outdf)
  }

  if(any(graphOrder == nonnumeric)){
    gind <- -which(graphOrder == nonnumeric)
    graphOrder <- graphOrder[gind]
    graphNames  <- graphNames[gind]
  }


  save(ts,file = file.path(webdir,"thisTs.Rdata"))

  #Initialize Rmd
  thisRmd <- "---" %>%
    str_c("output: html_document",sep = "\n") %>%
    str_c("---",sep = "\n") %>%
    str_c("\n")


  #write load data piece
  thisRmd <- thisRmd %>%
    str_c("```{r, echo = FALSE, warning = FALSE,message = FALSE}",sep = "\n") %>%
    str_c(glue('load("{file.path(webdir,"thisTs.Rdata")}")'),sep = "\n") %>%
    str_c("```",sep = "\n")

  thisRmd <- str_c(thisRmd,"PaleoData {.tabset .tabset-fade}",sep = "\n") %>%
    str_c("-----------------------------------------------------------------------",sep = "\n") %>%
    str_c("\n")


  for(cc in 1:length(graphOrder)){#for each column..
    #setup a new column in the markdown
    thisRmd <- str_c(thisRmd,str_c("### ",graphNames[cc]),sep = "\n") %>%
      str_c("```{r, echo = FALSE,warning = FALSE,message = FALSE}",sep = "\n") %>%
      str_c(str_c("lipdverseR::plotCol(ts,ind = ",as.character(graphOrder[cc]),")"),sep = "\n") %>%
      str_c("```",sep = "\n")  %>%
      str_c("\n")
  }



  #write out the Rmd
  write_file(thisRmd,path = file.path(webdir,"data",dsid,vers_,"paleoPlots.Rmd"))

  check <- try(rmarkdown::render(file.path(webdir,"data",dsid,vers_,"paleoPlots.Rmd")),silent = TRUE)
  if(class(check) ==  "try-error"){
    readr::write_file(x = " ",file = file.path(webdir,"data",dsid,vers_,"paleoPlots.html"))
    return("Paleo plots error")
  }


}

#create chrondata plots .html

createChronDataPlotHtml <- function(L,webdir = "/Volumes/data/Dropbox/lipdverse/html"){

  dsid <- L$datasetId
  dsn <- L$dataSetName
  vers <- sapply(L$changelog,"[[","version") %>% as.numeric_version() %>% max() %>% as.character()
  vers_ <- str_replace_all(vers,"[.]","_")

  if(length(L$chronData) < 1){
    readr::write_file(x = " ",file = file.path(webdir,"data",dsid,vers_,"chronPlots.html"))
    return("No chron")
  }

  if(length(L$chronData[[1]]$measurementTable) < 1){
    readr::write_file(x = " ",file = file.path(webdir,"data",dsid,vers_,"chronPlots.html"))
    return("No chron measurement table")
  }

  ts <- extractTs(L,mode = "chron",whichtables = "meas")

  thisVarNames <- sapply(ts,"[[","chronData_variableName")

  #graph order
  xcol <- which(startsWith(thisVarNames,"depth"))

  chronNum <-  pullTsVariable(ts,"chronNumber")
  tableNum <-  pullTsVariable(ts,"tableNumber")

  plotOrder <-  seq(1, length(ts)) #generate a sequence to start with
  isxcol <- plotOrder %in% xcol
  plotOrder <- plotOrder[order(chronNum,tableNum,-isxcol)]#sort by table Number


  ##create csv output for download
  lengths <- sapply(ts,function(x){length(x$chronData_values)})+1
  outdf <- data.frame(matrix(NA, nrow = max(lengths), ncol = length(lengths)))

  for(cc in 1:length(plotOrder)){#for each column..
    outdf[1,cc] <- ts[[plotOrder[cc]]]$chronData_TSid
    outdf[2:lengths[plotOrder[cc]],cc] <- ts[[plotOrder[cc]]]$chronData_values
    if(max(chronNum) == 1  & max(tableNum) == 1){
      names(outdf)[cc] <-str_c(ts[[plotOrder[cc]]]$chronData_variableName," (",ts[[plotOrder[cc]]]$chronData_units,")")
    }else{
      names(outdf)[cc] <-str_c(ts[[plotOrder[cc]]]$chronData_variableName," (",ts[[plotOrder[cc]]]$chronData_units,") [",as.character(ts[[plotOrder[cc]]]$chronNumber),"-",as.character(ts[[plotOrder[cc]]]$tableNumber),"]")
    }

    print(names(outdf)[cc])
  }

  if(!dir.exists(file.path(webdir,"data",dsid,vers_))){
    dir.create(file.path(webdir,"data",dsid,vers_),recursive = TRUE)
  }

  readr::write_csv(outdf,file.path(webdir,"data",dsid,vers_,paste0(dsn,"-chron.csv")))


  #check for non-numeric columns. Don't plot those.
  nonnumeric <- which(purrr::map_lgl(ts,~ !is.numeric(.x$chronData_values)))


  if(any(plotOrder %in% xcol)){#then remove those
    graphOrder = plotOrder[-which(plotOrder %in% xcol)]
    graphNames = names(outdf)[-which(plotOrder %in% xcol)]
  }else{
    graphOrder = plotOrder
    graphNames = names(outdf)
  }

  if(any(graphOrder == nonnumeric)){
    gind <- -which(graphOrder == nonnumeric)
    graphOrder <- graphOrder[gind]
    graphNames  <- graphNames[gind]
  }


  save(ts,file = file.path(webdir,"thisChronTs.Rdata"))

  #Initialize Rmd
  thisRmd <- "---" %>%
    str_c("output: html_document",sep = "\n") %>%
    str_c("---",sep = "\n") %>%
    str_c("\n")


  #write load data piece
  thisRmd <- thisRmd %>%
    str_c("```{r, echo = FALSE, warning = FALSE,message = FALSE}",sep = "\n") %>%
    str_c(glue('load("{file.path(webdir,"thisChronTs.Rdata")}")'),sep = "\n") %>%
    str_c("```",sep = "\n")

  thisRmd <- str_c(thisRmd,"ChronData {.tabset .tabset-fade}",sep = "\n") %>%
    str_c("-----------------------------------------------------------------------",sep = "\n") %>%
    str_c("\n")


  for(cc in 1:length(graphOrder)){#for each column..
    #setup a new column in the markdown
    thisRmd <- str_c(thisRmd,str_c("### ",graphNames[cc]),sep = "\n") %>%
      str_c("```{r, echo = FALSE,warning = FALSE,message = FALSE}",sep = "\n") %>%
      str_c(str_c("lipdverseR::plotCol(ts,ind = ",as.character(graphOrder[cc]),")"),sep = "\n") %>%
      str_c("```",sep = "\n")  %>%
      str_c("\n")
  }



  #write out the Rmd
  write_file(thisRmd,path = file.path(webdir,"data",dsid,vers_,"chronPlots.Rmd"))

  check <- try(rmarkdown::render(file.path(webdir,"data",dsid,vers_,"chronPlots.Rmd")),silent = TRUE)
  if(class(check) ==  "try-error"){
    readr::write_file(x = " ",file = file.path(webdir,"data",dsid,vers_,"chronPlots.html"))
    return("Chron error")
  }

}



#wrapper to create all for a LiPD file
createWebComponents <- function(L,webdir = "/Volumes/data/Dropbox/lipdverse/html"){
  if(is.null(L)){
    stop("This LiPD is empty")
  }
  createSidebarHtml(L,webdir = webdir)
  try(createPaleoDataPlotHtml(L,webdir = webdir))
  try(createChronDataPlotHtml(L,webdir = webdir))

}


#create iframe html for data page (or just lipdverse)
createDataWebPage <- function(L,webdir = "/Volumes/data/Dropbox/lipdverse/html"){
  dsid <- L$datasetId
  dsn <- L$dataSetName
  vers <- sapply(L$changelog,"[[","version") %>% as.numeric_version() %>% max() %>% as.character()
  vers_ <- str_replace_all(vers,"[.]","_")

  dsPath <- glue("../../{dsid}/{vers_}/")

  sidebarUrl <- "sidebar.html"
  mapUrl <- "../../../lipdverse/current_version/map.html"
  paleoUrl <- "paleoPlots.html"
  chronUrl <- "chronPlots.html"

  #create the components
  createWebComponents(L,webdir = webdir)


  #load in template iframe
  template <- readr::read_file(file.path(webdir,"iframeTemplateData.html"))

  #replace terms and links
  template <- template %>%
    str_replace("datasetIdHere",dsid) %>%
    str_replace("DatasetNameHere",dsn) %>%
    str_replace("sideBarUrlHere",sidebarUrl) %>%
    str_replace("mapUrlHere",mapUrl) %>%
    str_replace("paleoDataGraphsUrlHere",paleoUrl) %>%
    str_replace("chronDataGraphsUrlHere",chronUrl)

  readr::write_file(template,file = file.path(webdir,"data",dsid,vers_,paste0(dsn,".html")))

  #remove ensembles if present
  Lne <- removeEnsembles(L)

  if(object.size(L) > object.size(Lne)){#there's an ensemble
    writeLipd(L,path = file.path(webdir,"data",dsid,vers_,paste0(L$dataSetName,"-ensemble.lpd")))
    writeLipd(Lne,path = file.path(webdir,"data",dsid,vers_))
    writeLipd(Lne,path = file.path(webdir,"data",dsid,vers_),jsonOnly = TRUE)
    system(glue::glue("cp {file.path(webdir,'data',dsid,vers_,dsn)}-ensemble.lpd {file.path(webdir,'data',dsid,vers_)}/lipd-ensemble.lpd"))

  }else{
    writeLipd(L,path = file.path(webdir,"data",dsid,vers_))
    writeLipd(L,path = file.path(webdir,"data",dsid,vers_),jsonOnly = TRUE)
  }



  if(all(is.null(L$changelog))){
    L <- initializeChangelog(L)
  }

  addJsonldToHtml(htmlpath = file.path(webdir,"data",dsid,vers_,paste0(dsn,".html")),webdir = webdir)

  #write changelog html
  clpath <- file.path(webdir,'data',dsid,vers_,'changelog.md')
  clmd <- createMarkdownChangelog(L)
  readr::write_file(clmd,file = clpath)
  rmarkdown::render(clpath)

  #copy to index
  system(glue::glue("cp {file.path(webdir,'data',dsid,vers_,paste0(dsn,'.html'))} {file.path(webdir,'data',dsid,vers_)}/index.html"))

  #write dsid redirect html
  redirect <- readr::read_file(file.path(webdir,"redirectTemplate.html")) %>%
    str_replace("redirectUrlHere",glue::glue("{vers_}/index.html"))

  readr::write_file(redirect,file = file.path(webdir,"data",dsid,"index.html"))

  #copy lipd to generic
  system(glue::glue("cp {file.path(webdir,'data',dsid,vers_,dsn)}.lpd {file.path(webdir,'data',dsid,vers_)}/lipd.lpd"))
  system(glue::glue("cp {file.path(webdir,'data',dsid,vers_,dsn)}.jsonld {file.path(webdir,'data',dsid,vers_)}/lipd.jsonld"))

}


updateDataWebPageForCompilation <- function(L,webdir = "/Volumes/data/Dropbox/lipdverse/html"){
  dsid <- L$datasetId
  dsn <- L$dataSetName
  vers <- sapply(L$changelog,"[[","version") %>% as.numeric_version() %>% max() %>% as.character()
  vers_ <- str_replace_all(vers,"[.]","_")

  if(!file.exists(file.path(webdir,"data",dsid,"index.html"))){
    stop(glue::glue('It looks like {file.path(webdir,"data",dsid,"index.html")} does not exist. Perhaps you should use createDataWebPage()?'))
  }

  dsPath <- glue("../../{dsid}/{vers_}/")

  sidebarUrl <- "sidebar.html"
  mapUrl <- "../../map.html"
  paleoUrl <- "paleoPlots.html"
  chronUrl <- "chronPlots.html"

  #create the new sidebar
  createSidebarHtml(L,webdir = webdir)

  Lne <- removeEnsembles(L)

  if(object.size(L) > object.size(Lne)){#there's an ensemble
    writeLipd(L,path = file.path(webdir,"data",dsid,vers_,paste0(L$dataSetName,"-ensemble.lpd")))
    writeLipd(Lne,path = file.path(webdir,"data",dsid,vers_))
    writeLipd(Lne,path = file.path(webdir,"data",dsid,vers_),jsonOnly = TRUE)
    system(glue::glue("cp {file.path(webdir,'data',dsid,vers_,dsn)}-ensemble.lpd {file.path(webdir,'data',dsid,vers_)}/lipd-ensemble.lpd"))
  }else{
    #update the LiPD files
    writeLipd(L,path = file.path(webdir,"data",dsid,vers_))
    writeLipd(L,path = file.path(webdir,"data",dsid,vers_),jsonOnly = TRUE)
  }

  system(glue::glue("cp {file.path(webdir,'data',dsid,vers_,dsn)}.lpd {file.path(webdir,'data',dsid,vers_)}/lipd.lpd"))
  system(glue::glue("cp {file.path(webdir,'data',dsid,vers_,dsn)}.jsonld {file.path(webdir,'data',dsid,vers_)}/lipd.jsonld"))
}


createProjectDataWebPage <- function(dsid,
                                     dsn,
                                     vers,
                                     webdir = "/Volumes/data/Dropbox/lipdverse/html",
                                     project,
                                     projVersion){

  vers_ <- str_replace_all(vers,"[.]","_")

  dsPath <- glue("../../data/{dsid}/{vers_}/")
  projPath <- file.path(webdir,project,projVersion)

  sidebarUrl <- file.path(dsPath,"sidebar.html")
  mapUrl <- "map.html"
  paleoUrl <- file.path(dsPath,"paleoPlots.html")
  chronUrl <- file.path(dsPath,"chronPlots.html")

  #load in template iframe
  template <- readr::read_file(file.path(webdir,"iframeTemplate.html"))

  #replace terms and links
  template <- template %>%
    str_replace("datasetIdHere",dsid) %>%
    str_replace("DatasetNameHere",glue("{dsn} - {project} - {projVersion}")) %>%
    str_replace("sideBarUrlHere",sidebarUrl) %>%
    str_replace("mapUrlHere",mapUrl) %>%
    str_replace("paleoDataGraphsUrlHere",paleoUrl) %>%
    str_replace("chronDataGraphsUrlHere",chronUrl)

  readr::write_file(template,file = file.path(projPath,paste0(dsn,".html")))


}

#' Create an inventory data.frame
#'
#' @param D a multi-lipd object
#' @importFrom purrr map_chr
#' @return a data.frame
#' @export
createInventory <- function(D){
  inv <- data.frame(datasetId = purrr::map_chr(D,"datasetId"),
                    dataSetNameNew = purrr::map_chr(D,"dataSetName"),
                    dataSetVersion = purrr::map_chr(D,getVersion))

  return(inv)
}

#' Get the lipdverse inventory
#'
#' @param lipdDir the lipd directory of the database
#' @importFrom glue glue
#' @importFrom googledrive drive_find
#' @importFrom googlesheets4 read_sheet
#' @return a data.frame
#' @export
getInventory <- function(lipdDir,googEmail){
  invName <- paste0(basename(lipdDir),"-inventory")
  #googledrive::drive_auth(email = googEmail)
  tries <- 0
  while(TRUE){
    smatch <- try(R.utils::withTimeout({googledrive::drive_find(pattern = invName,n_max = 1)},
                                      timeout = 15,
                                      onTimeout = "error"),silent = TRUE)

    if(is(smatch,"try-error")){
      tries <- tries + 1
    }else{
      break
    }

    if(tries > 20){
      break
    }
  }

  if(nrow(smatch) == 0){
    createInventoryGoogle(lipdDir)
    while(TRUE){
      smatch <- try(R.utils::withTimeout({googledrive::drive_find(pattern = invName,n_max = 1)},
                                         timeout = 15,
                                         onTimeout = "error"),silent = TRUE)

      if(is(smatch,"try-error")){
        tries <- tries + 1
      }else{
        break
      }

      if(tries > 20){
        break
      }
    }
  }
  ss <- smatch$id
  #googlesheets4::gs4_auth(email = googEmail)
  return(googlesheets4::read_sheet(ss = ss,sheet = "inventory"))
}

#' Update the lipdverse inventory
#'
#' @param newInv the updated inventory data frame
#' @param lipdDir the lipd directory of the database
#' @importFrom glue glue
#' @importFrom googledrive drive_find
#' @importFrom googlesheets4 sheet_write
#' @importFrom dplyr filter
#' @export
updateInventory <- function(newInv,lipdDir){
  invName <- paste0(basename(lipdDir),"-inventory")
  smatch <- googledrive::drive_find(pattern = invName,n_max = 1)
  if(nrow(smatch) == 0){
    stop(glue::glue("No matches for {invName}, perhaps you need to use createInventory()?"))
  }
  ss <- smatch$id
  #check for and remove dummy data
  newInv <- dplyr::filter(newInv, -datasetId == "dumid")

  #update to google drive
  googlesheets4::sheet_write(ss = ss,newInv,sheet = "inventory")
}

#' Create a new inventory file on google drive
#'
#' @importFrom googlesheets4 gs4_create
#' @param newInv a inventory data frame
#' @param lipdDir the lipd directory of the database
#' @export
createInventoryGoogle <- function(lipdDir){
  invName <- paste0(basename(lipdDir),"-inventory")
  ss <- googlesheets4::gs4_create(name = invName,sheets = "inventory")
  newInv <- data.frame(datasetId = "dumid",	dataSetNameNew = "dumdsn", 	dataSetVersion = "1.0.0")
  googlesheets4::sheet_write(ss = ss,newInv,sheet = "inventory")
}

createProjectOverview <- function(D,TS,webDirectory,project,version){
  #if there's no html directory, create one
  if(!dir.exists(file.path(webDirectory,project))){
    dir.create(file.path(webDirectory,project))
  }

  if(!dir.exists(file.path(webDirectory,project,version))){
    dir.create(file.path(webDirectory,project,version))
  }


  sTS <- lipdR::splitInterpretationByScope(TS)

  save(list = c("D","TS","sTS"),file = file.path(webDirectory,project,version,str_c(project,version,".RData")))
  save(list = c("D","TS","sTS"),file = file.path(webDirectory,"temp.RData"))

  #remove columns we don't want to plot
  varNames <- sapply(TS, "[[","paleoData_variableName")

  #All datasets
  dsn <- lipdR::pullTsVariable(TS,"dataSetName")
  ui <- which(!duplicated(dsn))
  udsn <- dsn[ui]
  lat <- lipdR::pullTsVariable(TS,"geo_latitude")[ui]
  lon <- lipdR::pullTsVariable(TS,"geo_longitude")[ui]
  if("geo_elevation" %in% varNames){
    elev <- lipdR::pullTsVariable(TS,"geo_elevation")[ui]
  }else{
    elev <- matrix(NA, nrow = length(lat))

  }
  archiveType <- lipdR::pullTsVariable(TS,"archiveType")[ui]
  link <- paste0(udsn,".html") %>%
    str_replace_all("'","_")


  if(is.list(elev)){
    ge <- which(!sapply(elev,is.null))
    ne <- rep(NA,length(elev))
    ne[ge] <- as.numeric(unlist(elev))
    elev <- ge
  }

  #Organize metadata for map
  map.meta <- data.frame(dataSetName = udsn, #datasetname
                         lat = lat,#lat
                         lon = lon,#lon
                         # elev = elev,#elevation
                         archiveType = factor(archiveType),#archiveType
                         link = link)#Link

  #make project Rmd
  createProjectRmd(webDirectory,project, version )

  rmarkdown::render(file.path(webDirectory,project,version,"index.Rmd"))
  #add google tag
  tag <- readLines(file.path(webDirectory,"gatag.html"))

  message <- addGoogleTracker(file.path(webDirectory,project,version,"index.html"),tag)
  message2 <- addLogoLink(file.path(webDirectory,project,version,"index.html"),link = "https://lipdverse.org")
}




